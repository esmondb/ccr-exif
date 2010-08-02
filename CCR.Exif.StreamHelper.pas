{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.1 (2010-08-02)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.StreamHelper.pas.                                      }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.Exif.StreamHelper; 

interface

uses
  SysUtils, Classes{$IFNDEF UNICODE}, WideStrings{$ENDIF};

type
  {$IF not Declared(TBytes)}
  TBytes = array of Byte;           //added in D2007
  {$IFEND}
  {$IF not Declared(RawByteString)}
  RawByteString = AnsiString;       //added in D2009
  {$IFEND}
  {$IF not Declared(UnicodeString)}
  UnicodeString = type WideString;  //added in D2009
  {$IFEND}

  TUnicodeStringList = {$IFDEF UNICODE}TStringList{$ELSE}TWideStringList{$ENDIF};

  TEndianness = (SmallEndian, BigEndian);

  TStreamHelper = class helper for TStream
    function ReadByte(var Value: Byte): Boolean; overload;
    function ReadByte: Byte; overload;
    function ReadSmallInt(Endianness: TEndianness; var Value: SmallInt): Boolean; overload;
    function ReadSmallInt(Endianness: TEndianness): SmallInt; overload;
    function ReadWord(Endianness: TEndianness; var Value: Word): Boolean; overload;
    function ReadWord(Endianness: TEndianness): Word; overload;
    function ReadLongInt(Endianness: TEndianness; var Value: LongInt): Boolean; overload;
    function ReadLongInt(Endianness: TEndianness): LongInt; overload;
    function ReadLongWord(Endianness: TEndianness; var Value: LongWord): Boolean; overload;
    function ReadLongWord(Endianness: TEndianness): LongWord; overload;
    function ReadDouble(Endianness: TEndianness): Double;
    function ReadShortString(var S: ShortString): Boolean; overload;
    function ReadShortString: ShortString; overload;
    function TryReadBuffer(var Buffer; Count: Integer): Boolean;
    function TryReadHeader(const Header; HeaderSize: Byte): Boolean;
    procedure WriteByte(Value: Byte); overload;
    procedure WriteByte(Value: AnsiChar); overload;
    procedure WriteWord(Value: Word; Endianness: TEndianness);
    procedure WriteSmallInt(Value: SmallInt; Endianness: TEndianness);
    procedure WriteLongInt(Value: LongInt; Endianness: TEndianness);
    procedure WriteLongWord(Value: LongWord; Endianness: TEndianness);
    procedure WriteDouble(Value: Double; Endianness: TEndianness);
    procedure WriteUTF8Chars(const S: UTF8String); overload;
    procedure WriteUTF8Chars(const S: UnicodeString); overload; 
    procedure WriteUTF8Chars(const S: UnicodeString; const Args: array of const); overload;
    procedure WriteWideChars(const S: UnicodeString; Endianness: TEndianness);
  end;

  TUserMemoryStream = class(TCustomMemoryStream) //read-only stream access on an existing buffer;
  public
    constructor Create(Memory: Pointer; Size: Integer);
    procedure ChangeMemory(NewMemory: Pointer; NewSize: Integer);
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

function SwapLongInt(const Value: LongInt): LongInt;
function SwapLongWord(const Value: LongWord): LongWord;
function SwapSingle(const Value: Single): Single;
function SwapDouble(const Value: Double): Double;

function UnicodeFormat(const S: UnicodeString; const Args: array of const): UnicodeString;
function UnicodeSameText(const S1, S2: UnicodeString): Boolean; inline;

{$IFNDEF UNICODE}
function CharInSet(Ch: AnsiChar; const CharSet: TSysCharSet): Boolean; inline; overload;
function CharInSet(Ch: WideChar; const CharSet: TSysCharSet): Boolean; inline; overload;
function UpCase(const Ch: AnsiChar): AnsiChar; overload;
function UpCase(const Ch: WideChar): WideChar; overload;
{$ENDIF}

implementation

uses RTLConsts, SysConst;

function SwapLongInt(const Value: LongInt): LongInt;
asm
  BSWAP EAX
end;

function SwapLongWord(const Value: LongWord): LongWord;
asm
  BSWAP EAX
end;

function SwapSingle(const Value: Single): Single;
asm
  BSWAP EAX
end;

function SwapDouble(const Value: Double): Double;
var
  I: Integer;
begin
  for I := SizeOf(Double) - 1 downto 0 do
    PByteArray(@Result)[I] := PByteArray(@Value)[SizeOf(Double) - 1 - I];
end;

function UnicodeFormat(const S: UnicodeString; const Args: array of const): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := Format(S, Args);
{$ELSE}
  Result := WideFormat(S, Args);
{$ENDIF}
end;

function UnicodeSameText(const S1, S2: UnicodeString): Boolean; 
begin
{$IFDEF UNICODE}
  Result := SameText(S1, S2);
{$ELSE}
  Result := WideSameText(S1, S2);
{$ENDIF}
end;

{$IFNDEF UNICODE}
function CharInSet(Ch: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := Ch in CharSet;
end;

function CharInSet(Ch: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (Ch <= High(AnsiChar)) and (AnsiChar(Ch) in CharSet);
end;

function UpCase(const Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  case Result of
    'a'..'z':  Dec(Result, Ord('a') - Ord('A'));
  end;
end;

function UpCase(const Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Result of
    'a'..'z':  Dec(Result, Ord('a') - Ord('A'));
  end;
end;
{$ENDIF}

{ TStreamHelper }

function TStreamHelper.ReadByte(var Value: Byte): Boolean;
begin
  Result := (Read(Value, 1) = 1);
end;

function TStreamHelper.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;

function TStreamHelper.ReadWord(Endianness: TEndianness; var Value: Word): Boolean;
begin
  Result := (Read(Value, 2) = 2);
  if Result and (Endianness = BigEndian) then
    Value := Swap(Value)
end;

function TStreamHelper.ReadWord(Endianness: TEndianness): Word;
begin
  if not ReadWord(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.ReadDouble(Endianness: TEndianness): Double;
begin
  ReadBuffer(Result, 8);
  if Endianness = BigEndian then
    Result := SwapDouble(Result);
end;

function TStreamHelper.ReadLongInt(Endianness: TEndianness; var Value: LongInt): Boolean;
begin
  Result := (Read(Value, 4) = 4);
  if Result and (Endianness = BigEndian) then
    Value := SwapLongInt(Value);
end;

function TStreamHelper.ReadLongInt(Endianness: TEndianness): LongInt;
begin
  if not ReadLongInt(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.ReadLongWord(Endianness: TEndianness;
  var Value: LongWord): Boolean;
begin
  Result := (Read(Value, 4) = 4);
  if Result and (Endianness = BigEndian) then
    Value := SwapLongWord(Value);
end;

function TStreamHelper.ReadLongWord(Endianness: TEndianness): LongWord;
begin
  if not ReadLongWord(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.ReadShortString(var S: ShortString): Boolean;
begin
  Result := (Read(S[0], 1) = 1) and (Read(S[1], Byte(S[0])) = Byte(S[0]));
end;

function TStreamHelper.ReadShortString: ShortString;
begin
  if not ReadShortString(Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.ReadSmallInt(Endianness: TEndianness;
  var Value: SmallInt): Boolean;
begin
  Result := (Read(Value, 2) = 2);
  if Result and (Endianness = BigEndian) then
    Value := Swap(Value)
end;

function TStreamHelper.ReadSmallInt(Endianness: TEndianness): SmallInt;
begin
  if not ReadSmallInt(Endianness, Result) then
    raise EReadError.CreateRes(@SReadError);
end;

function TStreamHelper.TryReadBuffer(var Buffer; Count: Integer): Boolean;
begin
  Result := (Read(Buffer, Count) = Count)
end;

function TStreamHelper.TryReadHeader(const Header; HeaderSize: Byte): Boolean;
var
  Buffer: array[Byte] of Byte;
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, HeaderSize);
  Result := (BytesRead = HeaderSize) and CompareMem(@Buffer, @Header, HeaderSize);
  if not Result then Seek(-BytesRead, soCurrent);
end;

procedure TStreamHelper.WriteByte(Value: Byte);
begin
  WriteBuffer(Value, 1);
end;

procedure TStreamHelper.WriteByte(Value: AnsiChar);
begin
  WriteBuffer(Value, 1);
end;

procedure TStreamHelper.WriteDouble(Value: Double; Endianness: TEndianness);
begin
  if Endianness = BigEndian then
    Value := SwapDouble(Value);
  WriteBuffer(Value, 8);
end;

procedure TStreamHelper.WriteWideChars(const S: UnicodeString; Endianness: TEndianness);
var
  Ch: WideChar;
begin
  if Endianness = SmallEndian then
    WriteBuffer(Pointer(S)^, Length(S) * 2)
  else
    for Ch in S do
    begin
      WriteBuffer(WordRec(Ch).Hi, 1);
      WriteBuffer(WordRec(Ch).Lo, 1);
    end;
end;

procedure TStreamHelper.WriteWord(Value: Word; Endianness: TEndianness);
begin
  if Endianness = SmallEndian then
    WriteBuffer(Value, 2)
  else
  begin
    WriteBuffer(WordRec(Value).Hi, 1);
    WriteBuffer(WordRec(Value).Lo, 1);
  end;
end;

procedure TStreamHelper.WriteLongWord(Value: LongWord; Endianness: TEndianness);
begin
  if Endianness = BigEndian then
    Value := SwapLongWord(Value);
  WriteBuffer(Value, 4);
end;

procedure TStreamHelper.WriteSmallInt(Value: SmallInt; Endianness: TEndianness);
begin
  if Endianness = SmallEndian then
    WriteBuffer(Value, 2)
  else
  begin
    WriteBuffer(WordRec(Value).Hi, 1);
    WriteBuffer(WordRec(Value).Lo, 1);
  end;
end;

procedure TStreamHelper.WriteLongInt(Value: LongInt; Endianness: TEndianness);
begin
  if Endianness = BigEndian then
    Value := SwapLongInt(Value);
  WriteBuffer(Value, 4);
end;

procedure TStreamHelper.WriteUTF8Chars(const S: UTF8String);
begin
  WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TStreamHelper.WriteUTF8Chars(const S: UnicodeString);
begin
  WriteUTF8Chars(UTF8Encode(S));
end;

procedure TStreamHelper.WriteUTF8Chars(const S: UnicodeString;
  const Args: array of const);
begin
  WriteUTF8Chars(UTF8Encode(UnicodeFormat(S, Args)));
end;

{ TUserMemoryStream }

constructor TUserMemoryStream.Create(Memory: Pointer; Size: Integer);
begin
  inherited Create;
  SetPointer(Memory, Size);
end;

procedure TUserMemoryStream.ChangeMemory(NewMemory: Pointer; NewSize: Integer);
begin
  SetPointer(NewMemory, NewSize);
  Position := 0;
end;

function TUserMemoryStream.Write(const Buffer; Count: Integer): Integer;
begin
  Result := 0;
end;

end.
