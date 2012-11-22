/*
  Copyright (C) 2012-2013 the original author or authors.

  See the LICENSE.txt file distributed with this work for additional
  information regarding copyright ownership.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

package scalaopts.common.platform;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.win32.StdCallLibrary;

import java.awt.*;
import java.util.List;

import static scalaopts.common.platform.JNAUtils.fromSeq;

/**
 * Provides native access to platform-specific, terminal-related information.
 */
@SuppressWarnings("all")
public class Win32 {

  private static class Kernel32 implements StdCallLibrary {
    static {
      Native.register("kernel32");
    }

    public static final int INVALID_HANDLE_VALUE = -1;

    public static final int STD_INPUT_HANDLE = -10;
    public static final int STD_OUTPUT_HANDLE = -11;
    public static final int STD_ERROR_HANDLE = -12;

    public static final int FILE_SHARE_READ = 1;
    public static final int FILE_SHARE_WRITE = 2;

    public static final int GENERIC_READ  = 0x80000000;
    public static final int GENERIC_WRITE = 0x40000000;

    public static final int CONSOLE_TEXTMODE_BUFFER = 1;

    // http://msdn.microsoft.com/en-us/library/ms682093(VS.85).aspx
    public static class CONSOLE_SCREEN_BUFFER_INFO extends Structure {
      public short wSizeX;
      public short wSizeY;
      public short wCursorPositionX;
      public short wCursorPositionY;
      public short wAttributes;
      public short wWindowLeft;
      public short wWindowTop;
      public short wWindowRight;
      public short wWindowBottom;
      public short wMaximumWindowSizeX;
      public short wMaximumWindowSizeY;

      public static class ByReference extends CONSOLE_SCREEN_BUFFER_INFO implements Structure.ByReference {
      }

      public CONSOLE_SCREEN_BUFFER_INFO() {
      }

      public CONSOLE_SCREEN_BUFFER_INFO(Pointer memory) {
        super(memory);
        read();
      }

      @Override
      protected List getFieldOrder() {
        return fromSeq(
            "wSizeX"
            , "wSizeY"
            , "wCursorPositionX"
            , "wCursorPositionY"
            , "wAttributes"
            , "wWindowLeft"
            , "wWindowTop"
            , "wWindowRight"
            , "wWindowBottom"
            , "wMaximumWindowSizeX"
            , "wMaximumWindowSizeY"
        );
      }
    }

    public static native int CloseHandle(int hObject);

    public static native int GetStdHandle(int nStdHandle);

    // http://msdn.microsoft.com/en-us/library/ms683171(VS.85).aspx
    public static native int GetConsoleScreenBufferInfo(int hConsoleOutput, CONSOLE_SCREEN_BUFFER_INFO lpConsoleScreenBufferInfo);

    public static native int CreateConsoleScreenBuffer(int dwDesiredAccess, int dwShareMode, Pointer lpSecurityAttributes, int dwFlags, Pointer lpScreenBufferData);
  }

  public static class Terminal implements ITerminal {
    private static final Object lock = new Object();

    @Override
    public ITerminal.Dimension queryTerminalDimensions() {
      long width = DEFAULT_DIMENSIONS.Width;
      long height = DEFAULT_DIMENSIONS.Height;

      synchronized (lock) {
        final int handle = Kernel32.GetStdHandle(Kernel32.STD_OUTPUT_HANDLE);
        if (Kernel32.INVALID_HANDLE_VALUE != handle) {
          Kernel32.CONSOLE_SCREEN_BUFFER_INFO info = new Kernel32.CONSOLE_SCREEN_BUFFER_INFO();
          if (Kernel32.GetConsoleScreenBufferInfo(handle, info) != 0) {
            width = Math.max(0, info.wWindowRight - info.wWindowLeft + 1);
            height = Math.max(0, info.wWindowBottom - info.wWindowTop + 1);
          } else {
            //It's likely that STDOUT has been redirected, so create a new buffer, get the info.
            //we need, and then get out.
            final int new_buffer_handle = Kernel32.CreateConsoleScreenBuffer(Kernel32.GENERIC_READ | Kernel32.GENERIC_WRITE, Kernel32.FILE_SHARE_READ | Kernel32.FILE_SHARE_WRITE, Pointer.NULL, Kernel32.CONSOLE_TEXTMODE_BUFFER, Pointer.NULL);
            if (Kernel32.INVALID_HANDLE_VALUE != new_buffer_handle) {
              try {
                if (Kernel32.GetConsoleScreenBufferInfo(new_buffer_handle, info) != 0) {
                  width = Math.max(0, info.wWindowRight - info.wWindowLeft + 1);
                  height = Math.max(0, info.wWindowBottom - info.wWindowTop + 1);
                }
              } finally {
                Kernel32.CloseHandle(new_buffer_handle);
              }
            }
          }
          info = null;
        }
      }

      return new ITerminal.Dimension(width, height);
    }
  }
}
