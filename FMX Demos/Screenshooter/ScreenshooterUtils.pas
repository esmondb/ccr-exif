{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.2                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ScreenshooterUtils;

interface

uses
  System.Types, System.SysUtils, System.Classes, FMX.Types;

function GetUserDisplayName: string;
procedure TakeJpegScreenshot(Dest: TStream);

implementation

uses
  {$IFDEF MACOS}Macapi.CocoaTypes, Macapi.CoreFoundation, Macapi.CoreGraphics,
    Macapi.ImageIO, Macapi.AppKit;{$ENDIF}
  {$IFDEF MSWINDOWS}Winapi.Windows, Vcl.Graphics, Vcl.Imaging.Jpeg;{$ENDIF}

{$IFDEF MACOS}
function NSFullUserName: CFStringRef; cdecl; external
  '/System/Library/Frameworks/Foundation.framework/Foundation' name '_NSFullUserName';

function GetUserDisplayName: string;
var
  CFStr: CFStringRef;
  Range: CFRange;
begin
  CFStr := NSFullUserName;
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

{$IF NOT DECLARED(CGRectInfinite)}
const
  CGRectInfinite: CGRect = (origin: (x: -8.98847e+30; y: -8.98847e+307);
    size: (width: 1.79769e+308; height: 1.79769e+308));
{$IFEND}

function PutBytesCallback(Stream: TStream; NewBytes: Pointer; Count: LongInt): LongInt; cdecl;
begin
  Result := Stream.Write(NewBytes^, Count);
end;

procedure ReleaseConsumerCallback(Dummy: Pointer); cdecl;
begin
end;

procedure WriteCGImageToStream(const AImage: CGImageRef; AStream: TStream;
  const AType: string = 'public.jpeg'; AOptions: CFDictionaryRef = nil);
var
  Callbacks: CGDataConsumerCallbacks;
  Consumer: CGDataConsumerRef;
  ImageDest: CGImageDestinationRef;
  TypeCF: CFStringRef;
begin
  Callbacks.putBytes := @PutBytesCallback;
  Callbacks.releaseConsumer := ReleaseConsumerCallback;
  ImageDest := nil;
  TypeCF := nil;
  Consumer := CGDataConsumerCreate(AStream, @Callbacks);
  if Consumer = nil then RaiseLastOSError;
  try
    TypeCF := CFStringCreateWithCharactersNoCopy(nil, PChar(AType), Length(AType), kCFAllocatorNull);
    ImageDest := CGImageDestinationCreateWithDataConsumer(Consumer, TypeCF, 1, AOptions);
    if ImageDest = nil then RaiseLastOSError;
    CGImageDestinationAddImage(ImageDest, AImage, nil);
    if CGImageDestinationFinalize(ImageDest) = 0 then RaiseLastOSError;
  finally
    if ImageDest <> nil then CFRelease(ImageDest);
    if TypeCF <> nil then CFRelease(TypeCF);
    CGDataConsumerRelease(Consumer);
  end;
end;

procedure TakeJpegScreenshot(Dest: TStream);
var
  Screenshot: CGImageRef;
begin
  ScreenShot := CGWindowListCreateImage(CGRectInfinite,
    kCGWindowListOptionOnScreenOnly, kCGNullWindowID, kCGWindowImageDefault);
  if ScreenShot = nil then RaiseLastOSError;
  try
    WriteCGImageToStream(ScreenShot, Dest);
  finally
    CGImageRelease(ScreenShot);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TBitmap = FMX.Types.TBitmap;
  TVclBitmap = Vcl.Graphics.TBitmap;

function GetUserNameEx(NameFormat: DWORD; lpNameBuffer: LPTSTR; var nSize: ULONG): ByteBool; stdcall;
  external 'secur32.dll' Name 'GetUserNameExW';

const
  NameDisplay = 3;

function GetUserDisplayName: string;
var
  Buffer: array[Byte] of Char;
  Len: DWORD;
begin
  Len := Length(Buffer);
  if not GetUserNameEx(NameDisplay, Buffer, Len) then
    if not GetUserName(Buffer, Len) then
      RaiseLastOSError;
  SetString(Result, Buffer, Len)
end;

procedure TakeJpegScreenshot(Dest: TStream);
var
  Bitmap: TVclBitmap;
  DC: HDC;
  Jpeg: TJPEGImage;
  R: TRect;
begin
  if not GetWindowRect(GetDesktopWindow, R) then
    RaiseLastOSError;
  Bitmap := nil;
  Jpeg := nil;
  DC := GetDC(0);
  try
    Bitmap := TVclBitmap.Create;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(R.Width, R.Height);
    BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
      DC, 0, 0, SRCCOPY);
    Jpeg := TJPEGImage.Create;
    Jpeg.Assign(Bitmap);
    Jpeg.SaveToStream(Dest);
  finally
    ReleaseDC(0, DC);
    Jpeg.Free;
    Bitmap.Free;
  end;
end;
{$ENDIF}

end.
