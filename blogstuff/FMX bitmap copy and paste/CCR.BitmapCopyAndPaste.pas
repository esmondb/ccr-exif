unit CCR.BitmapCopyAndPaste;
{
  At my type of writing, FireMonkey is without a TClipboard implementation. One
  implication of this is that you can't copy or paste the contents of a TBitmap
  to the clipboard using the framework as it stands. This unit steps in to fill
  the gap - implementations are given for both OS X and Windows.

  Chris Rolliston, October 2011.
}
interface

uses
  {$IF DEFINED(MACOS)}
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit,
  {$ELSEIF DEFINED(MSWINDOWS)}
  Winapi.Windows,
  {$ELSE}
  {$MESSAGE FATAL 'Not yet implemented for the selected platform'}
  {$IFEND}
  System.SysUtils, System.Classes, FMX.Types;

function CanPasteBitmapFromClipboard: Boolean;
procedure CopyBitmapToClipboard(Bitmap: TBitmap);
function PasteBitmapFromClipboard(Bitmap: TBitmap): Boolean;

implementation

{$IF DEFINED(MACOS)}
type
  TUserMemoryStream = class(TCustomMemoryStream)
    constructor Create(AMemory: Pointer; const ALength: NativeInt);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

constructor TUserMemoryStream.Create(AMemory: Pointer; const ALength: NativeInt);
begin
  inherited Create;
  SetPointer(AMemory, ALength);
end;

function TUserMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function GeneralPasteboard: NSPasteboard; inline;
begin
  Result := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard);
end;

function NSObjectArray(const Objs: array of NSObject): NSArray;
var
  Counter: Integer;
  Obj: NSObject;
  Ptrs: array of Pointer;
begin
  SetLength(Ptrs, Length(Objs));
  Counter := 0;
  for Obj in Objs do
    if Obj <> nil then
    begin
      Ptrs[Counter] := (Obj as ILocalObject).GetObjectID;
      Inc(Counter);
    end;
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(Ptrs, Counter));
end;

function NSBitmapTypesArray: NSArray; inline;
begin
  Result := NSObjectArray([NSPasteboardTypePNG, NSPasteboardTypeTIFF]);
end;

function CanPasteBitmapFromClipboard: Boolean;
begin
  Result := (GeneralPasteboard.availableTypeFromArray(NSBitmapTypesArray) <> nil);
end;

procedure CopyBitmapToClipboard(Bitmap: TBitmap);
var
  Data: CFDataRef;
  Filter: TBitmapCodec;
  Pasteboard: NSPasteboard;
  Stream: TMemoryStream;
begin
  Data := nil;
  Stream := nil;
  Filter := DefaultBitmapCodecClass.Create;
  try
    Stream := TMemoryStream.Create;
    Filter.SaveToStream(Stream, Bitmap, 'png');
    Pasteboard := GeneralPasteboard;
    Pasteboard.declareTypes(NSObjectArray([NSPasteboardTypePNG]), nil);
    Data := CFDataCreateWithBytesNoCopy(nil, Stream.Memory, Stream.Size, kCFAllocatorNull);
    Pasteboard.setData(TNSData.Wrap(Data), NSPasteboardTypePNG);
  finally
    if Data <> nil then CFRelease(Data);
    Filter.Free;
    Stream.Free;
  end;
end;

function PasteBitmapFromClipboard(Bitmap: TBitmap): Boolean;
var
  Data: NSData;
  DataType: NSString;
  Pasteboard: NSPasteboard;
  Stream: TCustomMemoryStream;
begin
  Pasteboard := GeneralPasteboard;
  DataType := Pasteboard.availableTypeFromArray(NSBitmapTypesArray);
  if DataType <> nil then Data := Pasteboard.dataForType(DataType);
  if Data = nil then Exit(False);
  Stream := TUserMemoryStream.Create(Data.bytes, Data.length);
  try
    Bitmap.Clear(0); //needed due to FMX bug - try pasting the same PNG multiple times
    Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  Result := True;
end;
{$IFEND}

{$IF DEFINED(MSWINDOWS)}
function CanPasteBitmapFromClipboard: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_DIB);
end;

procedure CopyBitmapToClipboard(Bitmap: TBitmap);
var
  BitsSize: Integer;
  MemObj: HGLOBAL;
  Ptr: PBitmapInfoHeader;
begin
  BitsSize := Bitmap.Width * Bitmap.Height * 4;
  MemObj := GlobalAlloc(GMEM_MOVEABLE and GMEM_DDESHARE, SizeOf(TBitmapInfoHeader) + BitsSize);
  if MemObj = 0 then RaiseLastOSError;
  Ptr := GlobalLock(MemObj);
  if Ptr = nil then
  begin
    GlobalFree(MemObj);
    RaiseLastOSError;
  end;
  //fill out the info header
  FillChar(Ptr^, SizeOf(Ptr^), 0);
  Ptr.biSize := SizeOf(TBitmapInfoHeader);
  Ptr.biPlanes := 1;
  Ptr.biBitCount := 32;
  Ptr.biCompression := BI_RGB;
  Ptr.biWidth := Bitmap.Width;
  if Ptr.biWidth <= 0 then Ptr.biWidth := 1;
  Ptr.biHeight := -Bitmap.Height;
  if Ptr.biHeight >= 0 then Ptr.biHeight := -1;
  //copy over the image bits
  Inc(Ptr);
  if BitsSize <> 0 then
    Move(Bitmap.StartLine^, Ptr^, BitsSize);
  GlobalUnlock(MemObj);
  //assign the completed DIB memory object to the clipboard
  OpenClipboard(0);
  try
    EmptyClipboard;
    if not SetClipboardData(CF_DIB, MemObj) then
    begin
      GlobalFree(MemObj);
      RaiseLastOSError;
    end;
  finally
    CloseClipboard;
  end;
end;

function PasteBitmapFromClipboard(Bitmap: TBitmap): Boolean;
var
  Header: TBitmapFileHeader;
  MemObj: HGLOBAL;
  Ptr: PBitmapInfoHeader;
  Stream: TMemoryStream;
begin
  Ptr := nil;
  Stream := nil;
  OpenClipboard(0);
  try
    MemObj := GetClipboardData(CF_DIB);
    if MemObj = 0 then Exit(False);
    Ptr := GlobalLock(MemObj);
    if Ptr = nil then Exit(False);
    FillChar(Header, SizeOf(Header), 0);
    Header.bfType := $4D42;
    Header.bfSize := SizeOf(Header) + GlobalSize(MemObj);
    Header.bfOffBits := SizeOf(Header) + Ptr.biSize;
    Stream := TMemoryStream.Create;
    Stream.WriteBuffer(Header, SizeOf(Header));
    Stream.WriteBuffer(Ptr^, Header.bfSize - SizeOf(Header));
    Stream.Position := 0;
    Bitmap.LoadFromStream(Stream);
    Result := True;
  finally
    if Ptr <> nil then GlobalUnlock(MemObj);
    CloseClipboard;
    Stream.Free;
  end;
end;
{$IFEND}

end.
