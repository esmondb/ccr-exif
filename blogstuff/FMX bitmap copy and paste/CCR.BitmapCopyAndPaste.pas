unit CCR.BitmapCopyAndPaste;
{
  At my type of writing, FireMonkey is without a TClipboard implementation. One
  implication of this is that you can't copy or paste the contents of a TBitmap
  to the clipboard using the framework as it stands. This unit steps in to fill
  the gap - implementations are given for both OS X and Windows.

  Chris Rolliston, February 2012 (initial version October 2011).
}
interface

uses
  {$IF DEFINED(MACOS)}
  Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit,
  {$ELSEIF DEFINED(MSWINDOWS)}
  Winapi.Windows, Winapi.CommCtrl, Winapi.ShellApi, Vcl.Graphics,
  {$ELSE}
  {$MESSAGE FATAL 'Not yet implemented for the selected platform'}
  {$IFEND}
  System.SysUtils, System.Classes, FMX.Types;

function CanPasteBitmapFromClipboard: Boolean;
procedure CopyBitmapToClipboard(Bitmap: TBitmap);
function PasteBitmapFromClipboard(Bitmap: TBitmap): Boolean;

implementation

function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;
var
  Filter: TBitmapCodec;
begin
  Filter := DefaultBitmapCodecClass.Create;
  try
    Bitmap.Clear(0);
    if Filter.LoadFromFile(FileName, 0.0, Bitmap) then
    begin
      Bitmap.UpdateHandles;
      Bitmap.BitmapChanged;
      Exit(True);
    end;
  finally
    Filter.Free;
  end;
  Result := False;
end;

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
  Pasteboard: NSPasteboard;
  Stream: TMemoryStream;
begin
  Data := nil;
  Stream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(Stream);
    Pasteboard := GeneralPasteboard;
    Pasteboard.declareTypes(NSObjectArray([NSPasteboardTypePNG]), nil);
    Data := CFDataCreateWithBytesNoCopy(nil, Stream.Memory, Stream.Size, kCFAllocatorNull);
    Pasteboard.setData(TNSData.Wrap(Data), NSPasteboardTypePNG);
  finally
    if Data <> nil then CFRelease(Data);
    Stream.Free;
  end;
end;

function PasteBitmapFromClipboard(Bitmap: TBitmap): Boolean;
var
  CFStr: CFStringRef;
  Objs: NSArray;
  Data: NSData;
  DataType: NSString;
  FileName: string;
  Pasteboard: NSPasteboard;
  Range: CFRange;
  Stream: TStream;
  URL: NSURL;
begin
  Pasteboard := GeneralPasteboard;
  //try pasting the file contents first (TextEdit does this)
  Objs := Pasteboard.readObjectsForClasses(TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(objc_getClass('NSURL'))), nil);
  if (Objs <> nil) and (Objs.count > 0) then
  begin
    URL := TNSURL.Wrap(Objs.objectAtIndex(0));
    if URL.isFileURL then
    begin
      CFStr := (URL.path as ILocalObject).GetObjectID;
      Range.location := 0;
      Range.length := CFStringGetLength(CFStr);
      SetLength(FileName, Range.length);
      CFStringGetCharacters(CFStr, Range, PChar(FileName));
      if TryLoadBitmapFromFile(Bitmap, FileName) then Exit(True);
    end;
  end;
  //look for actual image data
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
const
  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';
  ImageListTypes: array[Boolean] of Integer = (SHIL_EXTRALARGE, SHIL_JUMBO);

function BackfillSHGetImageList(Flags: Integer; const IID: TGUID;
  var ImageList: HIMAGELIST): HRESULT; stdcall;
var
  Info: TSHFileInfo;
begin
  if IID <> IID_IImageList then Exit(E_NOINTERFACE);
  ImageList := SHGetFileInfo('', 0, Info, SizeOf(Info), SHGFI_SYSICONINDEX);
  if ImageList <> 0 then Exit(S_OK) else Exit(S_UNEXPECTED);
end;

{$J+}
function GetSysImageList(var ImageList: HIMAGELIST; var Width, Height: Integer): Boolean;
const
  LTriedToLoad: Boolean = False;
  LImageImage: HIMAGELIST = 0;
  LWidth: Integer = 0;
  LHeight: Integer = 0;
var
  SHGetImageList: function (Flags: Integer; const IID: TGUID; var ImageList: HIMAGELIST): HRESULT; stdcall;
begin
  if not LTriedToLoad then
  begin
    LTriedToLoad := True;
    SHGetImageList := GetProcAddress(GetModuleHandle('shell32.dll'), 'SHGetImageList');
    if @SHGetImageList = nil then
    begin
      SHGetImageList := GetProcAddress(GetModuleHandle('shell32.dll'), PChar(727));
      if @SHGetImageList = nil then SHGetImageList := BackfillSHGetImageList;
    end;
    if SHGetImageList(ImageListTypes[Win32MajorVersion >= 6], IID_IImageList, LImageImage) <> 0 then
      Exit(False);
    ImageList_GetIconSize(LImageImage, LWidth, LHeight);
  end;
  if LImageList = 0 then Exit(False);
  ImageList := LImageImage;
  Width := LWidth;
  Height := LHeight;
  Result := True;
end;

function CanPasteBitmapFromClipboard: Boolean;
const
  Formats: array[1..2] of UINT = (CF_DIB, CF_HDROP);
begin
  Result := (GetPriorityClipboardFormat(Formats, Length(Formats)) <> -1);
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
    if SetClipboardData(CF_DIB, MemObj) = 0 then
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
  FileName: array[0..MAX_PATH] of Char;
  FileInfo: TSHFileInfo;
  Header: TBitmapFileHeader;
  HDropObj, MemObj: HGLOBAL;
  ImageList: HIMAGELIST;
  BitmapInfoPtr: PBitmapInfoHeader;
  Stream: TMemoryStream;
  VclBitmap: Vcl.Graphics.TBitmap;
  Width, Height, Row: Integer;
begin
  MemObj := 0;
  BitmapInfoPtr := nil;
  Stream := nil;
  VclBitmap := nil;
  OpenClipboard(0);
  try
    //is there a file name on the clipboard that points to a graphic?
    HDropObj := GetClipboardData(CF_HDROP);
    if HDropObj <> 0 then
    begin
      DragQueryFile(HDropObj, 0, FileName, Length(FileName));
      if TryLoadBitmapFromFile(Bitmap, FileName) then Exit(True);
    end;
    //go for actual image data next
    MemObj := GetClipboardData(CF_DIB);
    if MemObj <> 0 then
    begin
      BitmapInfoPtr := GlobalLock(MemObj);
      if BitmapInfoPtr = nil then RaiseLastOSError;
      FillChar(Header, SizeOf(Header), 0);
      Header.bfType := $4D42;
      Header.bfSize := SizeOf(Header) + GlobalSize(MemObj);
      Header.bfOffBits := SizeOf(Header) + BitmapInfoPtr.biSize;
      Stream := TMemoryStream.Create;
      Stream.WriteBuffer(Header, SizeOf(Header));
      Stream.WriteBuffer(BitmapInfoPtr^, Header.bfSize - SizeOf(Header));
      Stream.Position := 0;
      Bitmap.LoadFromStream(Stream);
      Exit(True);
    end;
    //if a file name, mimic OS X and get the icon
    if (HDropObj <> 0) and GetSysImageList(ImageList, Width, Height) then
    begin
      SHGetFileInfo(FileName, 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX);
      VclBitmap := Vcl.Graphics.TBitmap.Create;
      VclBitmap.PixelFormat := pf32bit;
      VclBitmap.Canvas.Brush.Color := 0;
      VclBitmap.SetSize(Width, Height);
      ImageList_Draw(ImageList, FileInfo.iIcon, VclBitmap.Canvas.Handle, 0, 0, ILD_NORMAL);
      Bitmap.Clear(0);
      Bitmap.SetSize(Width, Height);
      for Row := 0 to Height - 1 do
        Move(VclBitmap.ScanLine[Row]^, Bitmap.ScanLine[Row]^, Width * 4);
      Bitmap.UpdateHandles;
      Bitmap.BitmapChanged;
      Exit(True);
    end;
    Result := False;
  finally
    if BitmapInfoPtr <> nil then GlobalUnlock(MemObj);
    CloseClipboard;
    Stream.Free;
    VclBitmap.Free;
  end;
end;
{$IFEND}

end.