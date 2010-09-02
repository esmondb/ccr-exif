{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.2 beta (2010-09-02)                                                      }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.IPTC.pas.                                              }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.Exif.IPTC;

interface
{
  As saved, IPTC data is a flat list of tags ('datasets'), no more no less, which is
  reflected in the implementation of TIPTCData.LoadFromStream. However, as found in JPEG
  files, they are put in an Adobe data structure, itself put inside an APP13 segment.

  Note that by default, string tags are 'only' interpreted as having UTF-8 data if the
  encoding tag is set, with the UTF-8 marker as its data. If you don't load any tags
  before adding others, however, the default is to persist to UTF-8, writing said marker
  tag of course. To force interpreting loaded tags as UTF-8, set the
  AlwaysAssumeUTF8Encoding property of TIPTCData to True *before* calling LoadFromJPEG
  or LoadFromStream.
}
uses
  Types, SysUtils, Classes, Graphics, JPEG,
  CCR.Exif.JPEGUtils, CCR.Exif.StreamHelper, CCR.Exif.TagIDs;

type
  TLongIntTagValue = record
  private
    FValue: LongInt;
    FMissingOrInvalid: Boolean;
    function GetAsString: string;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TLongIntTagValue; static;
    class operator Equal(const A, B: TLongIntTagValue): Boolean;
    class operator NotEqual(const A, B: TLongIntTagValue): Boolean;
    class operator Implicit(const Source: TLongIntTagValue): Int64;
    class operator Implicit(const Source: TLongIntTagValue): LongInt;
    class operator Implicit(const Source: LongInt): TLongIntTagValue;
    class operator LessThan(const A, B: TLongIntTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TLongIntTagValue): Boolean;
    class operator GreaterThan(const A, B: TLongIntTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TLongIntTagValue): Boolean;
    class operator Negative(const Source: TLongIntTagValue): TLongIntTagValue;
    property AsString: string read GetAsString;
    property MissingOrInvalid: Boolean read FMissingOrInvalid;
    property Value: LongInt read FValue;
  end;

  TLongWordTagValue = record
  private
    FValue: LongWord;
    FMissingOrInvalid: Boolean;
    function GetAsString: string;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TLongWordTagValue; static;
    class operator Equal(const A, B: TLongWordTagValue): Boolean;
    class operator NotEqual(const A, B: TLongWordTagValue): Boolean;
    class operator Implicit(const Source: TLongWordTagValue): Int64;
    class operator Implicit(const Source: TLongWordTagValue): UInt64;
    class operator Implicit(const Source: TLongWordTagValue): LongWord;
    class operator Implicit(const Source: LongWord): TLongWordTagValue;
    class operator LessThan(const A, B: TLongWordTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TLongWordTagValue): Boolean;
    class operator GreaterThan(const A, B: TLongWordTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TLongWordTagValue): Boolean;
    property AsString: string read GetAsString;
    property MissingOrInvalid: Boolean read FMissingOrInvalid;
    property Value: LongWord read FValue;
  end;

  TWordTagValue = record
  strict private
    FValue: Word;
    FMissingOrInvalid: Boolean;
    function GetAsString: string;
  public
    constructor CreateFromString(const AString: string);
    class function CreateMissingOrInvalid: TWordTagValue; static;
    class operator Equal(const A, B: TWordTagValue): Boolean;
    class operator NotEqual(const A, B: TWordTagValue): Boolean;
    class operator Implicit(const Source: TWordTagValue): Int64;
    class operator Implicit(const Source: TWordTagValue): UInt64;
    class operator Implicit(const Source: TWordTagValue): Integer;
    class operator Implicit(const Source: TWordTagValue): Word;
    class operator Implicit(const Source: TWordTagValue): TLongIntTagValue;
    class operator Implicit(const Source: TWordTagValue): TLongWordTagValue;
    class operator Implicit(const Source: Word): TWordTagValue;
    class operator LessThan(const A, B: TWordTagValue): Boolean;
    class operator LessThanOrEqual(const A, B: TWordTagValue): Boolean;
    class operator GreaterThan(const A, B: TWordTagValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TWordTagValue): Boolean;
    property AsString: string read GetAsString;
    property MissingOrInvalid: Boolean read FMissingOrInvalid;
    property Value: Word read FValue;
  end;

  EInvalidIPTCData = class(Exception);

  TStringDynArray = Types.TStringDynArray;

  TIPTCData = class;
  TIPTCSection = class;

  TIPTCTagID = type Byte;
  TIPTCTagIDs = set of TIPTCTagID;

  TIPTCTag = class //an instance represents a 'dataset' in IPTC jargon; instances need not be written in numerical order
  public type
    TChangeType = (ctID, ctData);
  private
    FData: Pointer;
    FDataSize: Integer;
    FID: TIPTCTagID;
    FSection: TIPTCSection;
    procedure SetDataSize(Value: Integer);
    procedure SetID(const Value: TIPTCTagID);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetIndex: Integer;
    procedure SetIndex(NewIndex: Integer);
  public
    destructor Destroy; override;
    procedure Assign(Source: TIPTCTag);
    procedure Changed(AType: TChangeType = ctData); overload; //call this if Data is modified directly
    procedure Delete;
    procedure UpdateData(const Buffer); overload; inline;
    procedure UpdateData(NewDataSize: Integer; const Buffer); overload;
    procedure UpdateData(NewDataSize: Integer; Source: TStream); overload;
    { ReadString treats the raw data as AnsiChar data, whatever the spec says. }
    function ReadString: string; overload;
    procedure WriteString(const NewValue: RawByteString); overload;
    procedure WriteString(const NewValue: UnicodeString); overload;
    { AsString assumes the underlying data type is as per the spec (unlike the case of
      Exif tag headers, IPTC ones do not specify their data type). }
    property AsString: string read GetAsString write SetAsString;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize write SetDataSize;
    property ID: TIPTCTagID read FID write SetID; //tag IDs need only be unique within sections 1, 7, 8 and 9
    property Index: Integer read GetIndex write SetIndex;
    property Section: TIPTCSection read FSection;
  end;

  TIPTCSectionID = 1..9;

{$Z1} //only TIPTCImageOrientation directly maps to the stored value
  TIPTCActionAdvised = (iaTagMissing, iaObjectKill, iaObjectReplace, iaObjectAppend, iaObjectReference);
  TIPTCImageOrientation = (ioTagMissing, ioLandscape = Ord('L'), ioPortrait = Ord('P'),
    ioSquare = Ord('S'));
  TIPTCPriority = (ipTagMissing, ipLowest, ipVeryLow, ipLow, ipNormal, ipNormalHigh,
    ipHigh, ipVeryHigh, ipHighest, ipUserDefined, ipReserved);

  TIPTCSection = class //an instance represents a 'record' in IPTC jargon
  public type
    TEnumerator = record
    strict private
      FIndex: Integer;
      FSource: TIPTCSection;
      function GetCurrent: TIPTCTag;
    public
      constructor Create(ASection: TIPTCSection);
      function MoveNext: Boolean;
      property Current: TIPTCTag read GetCurrent;
    end;
  private
    FDefinitelySorted: Boolean;
    FID: TIPTCSectionID;
    FModified: Boolean;
    FOwner: TIPTCData;
    FTags: TList;
    function GetTag(Index: Integer): TIPTCTag;
    function GetTagCount: Integer;
  public
    constructor Create(AOwner: TIPTCData; AID: TIPTCSectionID);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator;
    function Add(ID: TIPTCTagID): TIPTCTag; //will try to insert in an appropriate place
    function Append(ID: TIPTCTagID): TIPTCTag; //will literally just append
    procedure Clear;
    procedure Delete(Index: Integer);
    function Find(ID: TIPTCTagID; out Tag: TIPTCTag): Boolean;
    function Insert(Index: Integer; ID: TIPTCTagID = 0): TIPTCTag;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(TagID: TIPTCTagID): Integer; overload; inline;
    function Remove(TagIDs: TIPTCTagIDs): Integer; overload;
    procedure Sort;
    function TagExists(ID: TIPTCTagID; MinSize: Integer = 1): Boolean;
    function AddOrUpdate(TagID: TIPTCTagID; NewDataSize: LongInt; const Buffer): TIPTCTag;
    function GetDateValue(TagID: TIPTCTagID): TDateTime;
    procedure SetDateValue(TagID: TIPTCTagID; const Value: TDateTime);
    function GetPriorityValue(TagID: TIPTCTagID): TIPTCPriority;
    procedure SetPriorityValue(TagID: TIPTCTagID; Value: TIPTCPriority);
    function GetRepeatableValue(TagID: TIPTCTagID): TStringDynArray; overload;
    procedure GetRepeatableValue(TagID: TIPTCTagID; Dest: TStrings; ClearDestFirst: Boolean = True); overload;
    procedure SetRepeatableValue(TagID: TIPTCTagID; const Value: array of string); overload;
    procedure SetRepeatableValue(TagID: TIPTCTagID; Value: TStrings); overload;
    procedure GetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Dest: TStrings; ClearDestFirst: Boolean = True); overload;
    procedure SetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Pairs: TStrings); overload;
    function GetStringValue(TagID: TIPTCTagID): string;
    procedure SetStringValue(TagID: TIPTCTagID; const Value: string);
    function GetWordValue(TagID: TIPTCTagID): TWordTagValue;
    procedure SetWordValue(TagID: TIPTCTagID; const Value: TWordTagValue);
    property Count: Integer read GetTagCount;
    property ID: TIPTCSectionID read FID;
    property Modified: Boolean read FModified write FModified;
    property Owner: TIPTCData read FOwner;
    property Tags[Index: Integer]: TIPTCTag read GetTag; default;
  end;

  TIPTCData = class(TInterfacedPersistent, IStreamPersist)
  public type
    TEnumerator = record
    private
      FCurrentID: TIPTCSectionID;
      FDoneFirst: Boolean;
      FSource: TIPTCData;
        function GetCurrent: TIPTCSection;
    public
      constructor Create(ASource: TIPTCData);
      function MoveNext: Boolean;
      property Current: TIPTCSection read GetCurrent;
    end;
  strict private
    FAlwaysAssumeUTF8Encoding: Boolean;
    FSections: array[TIPTCSectionID] of TIPTCSection;
    FUTF8Encoded: Boolean;
    function GetEmpty: Boolean;
    function GetSection(ID: TIPTCSectionID): TIPTCSection;
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    procedure SetUTF8Encoded(Value: Boolean);
    function GetEnvelopeString(TagID: Integer): string;
    procedure SetEnvelopeString(TagID: Integer; const Value: string);
    function GetEnvelopeWord(TagID: Integer): TWordTagValue;
    procedure SetEnvelopeWord(TagID: Integer; const Value: TWordTagValue);
    function GetApplicationWord(TagID: Integer): TWordTagValue;
    procedure SetApplicationWord(TagID: Integer; const Value: TWordTagValue);
    function GetApplicationString(TagID: Integer): string;
    procedure SetApplicationString(TagID: Integer; const Value: string);
    function GetApplicationStrings(TagID: Integer): TStringDynArray;
    procedure SetApplicationStrings(TagID: Integer; const Value: TStringDynArray);
    function GetUrgency: TIPTCPriority;
    procedure SetUrgency(Value: TIPTCPriority);
    function GetEnvelopePriority: TIPTCPriority;
    procedure SetEnvelopePriority(const Value: TIPTCPriority);
    function GetDate(PackedIndex: Integer): TDateTime;
    procedure SetDate(PackedIndex: Integer; const Value: TDateTime);
    function GetActionAdvised: TIPTCActionAdvised;
    procedure SetActionAdvised(Value: TIPTCActionAdvised);
    function GetImageOrientation: TIPTCImageOrientation;
    procedure SetImageOrientation(Value: TIPTCImageOrientation);
  protected
    function CreateIPTCSegment: IJPEGSegment;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoSaveToJPEG(InStream, OutStream: TStream);
  public const
    UTF8Marker: array[1..3] of Byte = ($1B, $25, $47);
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator; overload;
    procedure AddFromStream(Stream: TStream);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure LoadFromJPEG(JPEGStream: TStream); overload;
    procedure LoadFromJPEG(JPEGImage: TJPEGImage); overload;
    procedure LoadFromJPEG(const FileName: string); overload;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToJPEG(const JPEGFileName: string; BufferedWrite: Boolean = True); overload; //file must already exist
    procedure SaveToJPEG(JPEGImage: TJPEGImage); overload;
    procedure SaveToStream(Stream: TStream); overload;
    procedure SortTags;
    property Empty: Boolean read GetEmpty;
    property Modified: Boolean read GetModified write SetModified;
    property EnvelopeSection: TIPTCSection read FSections[1];
    property ApplicationSection: TIPTCSection read FSections[2];
    property FirstDescriptorSection: TIPTCSection read FSections[7];
    property ObjectDataSection: TIPTCSection read FSections[8];
    property SecondDescriptorSection: TIPTCSection read FSections[9];
    property Sections[ID: TIPTCSectionID]: TIPTCSection read GetSection; default;
  published
    property AlwaysAssumeUTF8Encoding: Boolean read FAlwaysAssumeUTF8Encoding write FAlwaysAssumeUTF8Encoding default False;
    property UTF8Encoded: Boolean read FUTF8Encoded write SetUTF8Encoded default True;
    { record 1 }
    property ModelVersion: TWordTagValue index itModelVersion read GetEnvelopeWord write SetEnvelopeWord stored False;
    property Destination: string index itDestination read GetEnvelopeString write SetEnvelopeString stored False;
    property FileFormat: TWordTagValue index itFileFormat read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    property FileFormatVersion: TWordTagValue index itFileFormatVersion read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    property ServiceIdentifier: string index itServiceIdentifier read GetEnvelopeString write SetEnvelopeString stored False;
    property EnvelopeNumberString: string index itEnvelopeNumber read GetEnvelopeString write SetEnvelopeString stored False;
    property ProductID: string index itProductID read GetEnvelopeString write SetEnvelopeString stored False;
    property EnvelopePriority: TIPTCPriority read GetEnvelopePriority write SetEnvelopePriority stored False;
    property DateSent: TDateTime index isEnvelope or itDateSent shl 8 read GetDate write SetDate;
    property UNOCode: string index itUNO read GetEnvelopeString write SetEnvelopeString stored False; //should have a specific format
    property ARMIdentifier: TWordTagValue index itARMIdentifier read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    property ARMVersion: TWordTagValue index itARMVersion read GetEnvelopeWord write SetEnvelopeWord stored False; //!!!make an enum
    { record 2 }
    property RecordVersion: TWordTagValue index itRecordVersion read GetApplicationWord write SetApplicationWord stored False;
    property ObjectTypeRef: string index itObjectTypeRef read GetApplicationString write SetApplicationString stored False;
    property ObjectAttributeRef: string index itObjectAttributeRef read GetApplicationString write SetApplicationString stored False;
    property ObjectName: string index itObjectName read GetApplicationString write SetApplicationString stored False;
    property EditStatus: string index itEditStatus read GetApplicationString write SetApplicationString stored False;
    property Urgency: TIPTCPriority read GetUrgency write SetUrgency stored False;
    property SubjectRefs: TStringDynArray index itSubjectRef read GetApplicationStrings write SetApplicationStrings stored False;
    property CategoryCode: string index itCategory read GetApplicationString write SetApplicationString stored False; //should be a 3 character code
    property SupplementaryCategories: TStringDynArray index itSupplementaryCategory read GetApplicationStrings write SetApplicationStrings stored False;
    property FixtureIdentifier: string index itFixtureIdentifier read GetApplicationString write SetApplicationString stored False;
    property Keywords: TStringDynArray index itKeyword read GetApplicationStrings write SetApplicationStrings stored False;
    procedure GetKeywords(Dest: TStrings); overload;
    procedure SetKeywords(NewWords: TStrings); overload;
    property ContentLocationCodes: TStringDynArray index itContentLocationCode read GetApplicationStrings write SetApplicationStrings stored False;
    property ContentLocationNames: TStringDynArray index itContentLocationName read GetApplicationStrings write SetApplicationStrings stored False;
    procedure GetContentLocationValues(Strings: TStrings); //Code=Name
    procedure SetContentLocationValues(Strings: TStrings);
    property ReleaseDate: TDateTime index isApplication or itReleaseDate shl 8 read GetDate write SetDate stored False;
    property ExpirationDate: TDateTime index isApplication or itExpirationDate shl 8 read GetDate write SetDate stored False;
    property SpecialInstructions: string index itSpecialInstructions read GetApplicationString write SetApplicationString stored False;
    property ActionAdvised: TIPTCActionAdvised read GetActionAdvised write SetActionAdvised stored False;
    property DateCreated: TDateTime index isApplication or itDateCreated shl 8 read GetDate write SetDate stored False;
    property DigitalCreationDate: TDateTime index isApplication or itDigitalCreationDate shl 8 read GetDate write SetDate stored False;
    property OriginatingProgram: string index itOriginatingProgram read GetApplicationString write SetApplicationString stored False;
    property ProgramVersion: string index itProgramVersion read GetApplicationString write SetApplicationString stored False;
    property ObjectCycleCode: string index itObjectCycle read GetApplicationString write SetApplicationString stored False; //!!!enum
    property Bylines: TStringDynArray index itByline read GetApplicationStrings write SetApplicationStrings stored False;
    property BylineTitles: TStringDynArray index itBylineTitle read GetApplicationStrings write SetApplicationStrings stored False;
    procedure GetBylineValues(Strings: TStrings); //Name=Title
    procedure SetBylineValues(Strings: TStrings);
    property City: string index itCity read GetApplicationString write SetApplicationString stored False; //!!!enum
    property SubLocation: string index itSubLocation read GetApplicationString write SetApplicationString stored False; //!!!enum
    property ProvinceOrState: string index itProvinceOrState read GetApplicationString write SetApplicationString stored False; //!!!enum
    property CountryCode: string index itCountryCode read GetApplicationString write SetApplicationString stored False; //!!!enum
    property CountryName: string index itCountryName read GetApplicationString write SetApplicationString stored False; //!!!enum
    property OriginalTransmissionRef: string index itOriginalTransmissionRef read GetApplicationString write SetApplicationString stored False; //!!!enum
    property Headline: string index itHeadline read GetApplicationString write SetApplicationString stored False;
    property Credit: string index itCredit read GetApplicationString write SetApplicationString stored False;
    property Source: string index itSource read GetApplicationString write SetApplicationString stored False;
    property CopyrightNotice: string index itCopyrightNotice read GetApplicationString write SetApplicationString stored False;
    property Contacts: TStringDynArray index itContact read GetApplicationStrings write SetApplicationStrings stored False;
    property CaptionOrAbstract: string index itCaptionOrAbstract read GetApplicationString write SetApplicationString stored False;
    property WritersOrEditors: TStringDynArray index itWriterOrEditor read GetApplicationStrings write SetApplicationStrings stored False;
    property ImageTypeCode: string index itImageType read GetApplicationString write SetApplicationString stored False;
    property ImageOrientation: TIPTCImageOrientation read GetImageOrientation write SetImageOrientation stored False;
    property LanguageIdentifier: string index itLanguageIdentifier read GetApplicationString write SetApplicationString stored False;
    property AudioTypeCode: string index itAudioType read GetApplicationString write SetApplicationString stored False;
  end;

function BinToHexStr(Data: Pointer; Size: Integer): string; overload;
function BinToHexStr(const Buffer; Size: Integer): string; overload; inline;

implementation

uses Contnrs, Math, CCR.Exif.Consts;

{$IFOPT R-}
  {$DEFINE RANGECHECKINGOFF}
{$ENDIF}

const
  PriorityChars: array[TIPTCPriority] of AnsiChar = (#0, '8', '7', '6', '5', '4',
    '3', '2', '1', '9', '0');

type
  TIPTCTagDataType = (idString, idWord, idBinary);

function BinToHexStr(Data: Pointer; Size: Integer): string;
begin
  SetString(Result, nil, 0);
  if Size = 0 then Exit;
  SetString(Result, nil, Size * 2);
  BinToHex(PAnsiChar(Data), PChar(Result), Size);
end;

function BinToHexStr(const Buffer; Size: Integer): string;
begin
  Result := BinToHexStr(@Buffer, Size);
end;

function FindProperDataType(Tag: TIPTCTag): TIPTCTagDataType;
begin
  Result := idString;
  if Tag.Section <> nil then
    case Tag.Section.ID of
      isEnvelope:
        case Tag.ID of
          itModelVersion, itFileFormat, itFileFormatVersion, itARMIdentifier,
          itARMVersion: Result := idWord;
        end;
      isApplication:
        case Tag.ID of
          itRecordVersion: Result := idWord;
        end;
    end;
end;

{ TLongIntTagValue }

constructor TLongIntTagValue.CreateFromString(const AString: string);
begin
  if not TryStrToInt(AString, FValue) then
    Self := CreateMissingOrInvalid;
end;

class function TLongIntTagValue.CreateMissingOrInvalid;
begin
  Result.FMissingOrInvalid := True;
  Result.FValue := 0;
end;

function TLongIntTagValue.GetAsString: string;
begin
  if FMissingOrInvalid then
    Result := ''
  else
    Result := IntToStr(FValue);
end;

class operator TLongIntTagValue.Equal(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.Value = B.Value) and (A.MissingOrInvalid = B.MissingOrInvalid);
end;

class operator TLongIntTagValue.Implicit(const Source: TLongIntTagValue): Int64;
begin
  Result := Source.Value;
end;

class operator TLongIntTagValue.Implicit(const Source: TLongIntTagValue): LongInt;
begin
  Result := Source.Value;
end;

class operator TLongIntTagValue.Implicit(const Source: LongInt): TLongIntTagValue;
begin
  Result.FValue := Source;
  Result.FMissingOrInvalid := False;
end;

class operator TLongIntTagValue.LessThan(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid;
end;

class operator TLongIntTagValue.LessThanOrEqual(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongIntTagValue.NotEqual(const A, B: TLongIntTagValue): Boolean;
begin
  Result := not (A = B);
end;

class operator TLongIntTagValue.GreaterThan(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid;
end;

class operator TLongIntTagValue.GreaterThanOrEqual(const A, B: TLongIntTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongIntTagValue.Negative(const Source: TLongIntTagValue): TLongIntTagValue;
begin
  if Source.MissingOrInvalid then
    Result := Source
  else
    Result := -Source.Value;
end;

{ TLongWordTagValue }

constructor TLongWordTagValue.CreateFromString(const AString: string);
var
  Int64Val: Int64;
begin
  if not TryStrToInt64(AString, Int64Val) or (Int64Val < 0) or (Int64Val > High(LongWord)) then
    Self := CreateMissingOrInvalid;
end;

class function TLongWordTagValue.CreateMissingOrInvalid;
begin
  Result.FMissingOrInvalid := True;
  Result.FValue := 0;
end;

class operator TLongWordTagValue.Equal(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.Value = B.Value) and (A.MissingOrInvalid = B.MissingOrInvalid);
end;

function TLongWordTagValue.GetAsString: string;
begin
  if FMissingOrInvalid then
    Result := ''
  else
    Result := IntToStr(FValue);
end;

class operator TLongWordTagValue.GreaterThan(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not B.MissingOrInvalid; //don't need check for A.MissingOrInvalid
end;

class operator TLongWordTagValue.GreaterThanOrEqual(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongWordTagValue.Implicit(const Source: TLongWordTagValue): Int64;
begin
  Result := Source.Value;
end;

class operator TLongWordTagValue.Implicit(const Source: TLongWordTagValue): UInt64;
begin
  Result := Source.Value;
end;

class operator TLongWordTagValue.Implicit(const Source: TLongWordTagValue): LongWord;
begin
  Result := Source.Value;
end;

class operator TLongWordTagValue.Implicit(const Source: LongWord): TLongWordTagValue;
begin
  Result.FValue := Source;
  Result.FMissingOrInvalid := False;
end;

class operator TLongWordTagValue.LessThan(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid;
end;

class operator TLongWordTagValue.LessThanOrEqual(const A, B: TLongWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TLongWordTagValue.NotEqual(const A, B: TLongWordTagValue): Boolean;
begin
  Result := not (A = B);
end;

{ TWordTagValue }

constructor TWordTagValue.CreateFromString(const AString: string);
var
  IntVal: Integer;
begin
  if not TryStrToInt(AString, IntVal) or (IntVal < 0) or (IntVal > High(Word)) then
    Self := CreateMissingOrInvalid;
end;

class function TWordTagValue.CreateMissingOrInvalid;
begin
  Result.FMissingOrInvalid := True;
  Result.FValue := 0;
end;

class operator TWordTagValue.Equal(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.Value = B.Value) and (A.MissingOrInvalid = B.MissingOrInvalid);
end;

function TWordTagValue.GetAsString: string;
begin
  if FMissingOrInvalid then
    Result := ''
  else
    Result := IntToStr(FValue);
end;

class operator TWordTagValue.GreaterThan(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.Value > B.Value) and not B.MissingOrInvalid; //don't need check for A.MissingOrInvalid
end;

class operator TWordTagValue.GreaterThanOrEqual(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value >= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): TLongIntTagValue;
begin
  if Source.MissingOrInvalid then
    Result := TLongIntTagValue.CreateMissingOrInvalid
  else
    Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): TLongWordTagValue;
begin
  if Source.MissingOrInvalid then
    Result := TLongWordTagValue.CreateMissingOrInvalid
  else
    Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): Int64;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): UInt64;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): Integer;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: TWordTagValue): Word;
begin
  Result := Source.Value;
end;

class operator TWordTagValue.Implicit(const Source: Word): TWordTagValue;
begin
  Result.FValue := Source;
  Result.FMissingOrInvalid := False;
end;

class operator TWordTagValue.LessThan(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.Value < B.Value) and not A.MissingOrInvalid;
end;

class operator TWordTagValue.LessThanOrEqual(const A, B: TWordTagValue): Boolean;
begin
  Result := (A.MissingOrInvalid and B.MissingOrInvalid) or
    ((A.Value <= B.Value) and not A.MissingOrInvalid and not B.MissingOrInvalid);
end;

class operator TWordTagValue.NotEqual(const A, B: TWordTagValue): Boolean;
begin
  Result := not (A = B);
end;

{ TIPTCTag }

destructor TIPTCTag.Destroy;
begin
  if FSection <> nil then FSection.FTags.Extract(Self);
  ReallocMem(FData, 0);
  inherited;
end;

procedure TIPTCTag.Assign(Source: TIPTCTag);
begin
  if Source = nil then
    SetDataSize(0)
  else
  begin
    FID := Source.ID;
    UpdateData(Source.DataSize, Source.Data^);
  end;
end;

procedure TIPTCTag.Changed(AType: TChangeType);
begin
  if FSection = nil then Exit;
  FSection.Modified := True;
  if AType = ctID then FSection.FDefinitelySorted := False;
end;

procedure TIPTCTag.Delete;
begin
  Free;
end;

function TIPTCTag.GetAsString: string;
begin
  case FindProperDataType(Self) of
    idString: Result := ReadString;
    idBinary: Result := BinToHexStr(FData, FDataSize);
  else
    case DataSize of
      1: Result := IntToStr(PByte(Data)^);
      2: Result := IntToStr(Swap(PWord(Data)^));
      4: Result := IntToStr(SwapLongInt(PLongInt(Data)^));
    else Result := ReadString;
    end;
  end;
end;

function TIPTCTag.GetIndex: Integer;
begin
  if FSection = nil then
    Result := -1
  else
    Result := FSection.FTags.IndexOf(Self);
end;

//procedure TIPTCTag.ReadString(var Result: AnsiString);
//begin
//  if (Section <> nil) and (Section.Owner <> nil) and Section.Owner.UTF8Encoded then
//  else
//    SetString(Result, PAnsiChar(FData), FDataSize);
//end;
//
//function TIPTCTag.ReadString: string;
//{$IFDEF UNICODE}
//var
//  AnsiStr: AnsiString;
//begin
//  ReadString(AnsiStr);
//  Result := string(AnsiStr);
//end;
//{$ELSE}
//begin
//  ReadString(Result)
//end;
//{$ENDIF}

function TIPTCTag.ReadString: string;
var
  Ansi: AnsiString;
  UTF8: UTF8String;
begin
  if (Section <> nil) and (Section.Owner <> nil) and Section.Owner.UTF8Encoded then
  begin
    SetString(UTF8, PAnsiChar(FData), FDataSize);
    {$IFDEF UNICODE}
    Result := string(UTF8);
    {$ELSE}
    Result := Utf8ToAnsi(UTF8);
    {$ENDIF}
    Exit;
  end;
  SetString(Ansi, PAnsiChar(FData), FDataSize);
  Result := string(Ansi);
end;

procedure TIPTCTag.SetAsString(const Value: string);
var
  WordVal: Integer;
begin
  case FindProperDataType(Self) of
    idString: WriteString(Value);
    idBinary:
    begin
      SetDataSize(Length(Value) div 2);
      HexToBin(PChar(LowerCase(Value)), FData, FDataSize);
    end
  else
    {$RANGECHECKS ON}
    WordVal := StrToInt(Value);
    {$IFDEF RANGECHECKINGOFF}{$RANGECHECKS OFF}{$ENDIF}
    WordVal :=  Swap(WordVal);
    UpdateData(2, WordVal);
  end;
end;

procedure TIPTCTag.SetDataSize(Value: Integer);
begin
  if Value = FDataSize then Exit;
  ReallocMem(FData, Value);
  FDataSize := Value;
  Changed;
end;

procedure TIPTCTag.SetID(const Value: TIPTCTagID);
begin
  if Value = FID then Exit;
  FID := Value;
  Changed(ctID);
end;

procedure TIPTCTag.SetIndex(NewIndex: Integer);
begin
  if FSection <> nil then FSection.Move(Index, NewIndex);
end;

procedure TIPTCTag.UpdateData(const Buffer);
begin
  UpdateData(DataSize, Buffer);
end;

procedure TIPTCTag.UpdateData(NewDataSize: Integer; const Buffer);
begin
  ReallocMem(FData, NewDataSize);
  FDataSize := NewDataSize;
  Move(Buffer, FData^, NewDataSize);
  Changed;
end;

procedure TIPTCTag.UpdateData(NewDataSize: Integer; Source: TStream);
begin
  ReallocMem(FData, NewDataSize);
  FDataSize := NewDataSize;
  Source.Read(FData^, NewDataSize);
  Changed;
end;

procedure TIPTCTag.WriteString(const NewValue: RawByteString);
begin
  ReallocMem(FData, 0);
  FDataSize := Length(NewValue);
  if FDataSize <> 0 then
  begin
    ReallocMem(FData, FDataSize);
    Move(Pointer(NewValue)^, FData^, FDataSize);
  end;
  Changed;
end;

procedure TIPTCTag.WriteString(const NewValue: UnicodeString);
begin
  if (Section <> nil) and (Section.Owner <> nil) and Section.Owner.UTF8Encoded then
    WriteString(UTF8Encode(NewValue))
  else
    WriteString(AnsiString(NewValue));
end;

{ TIPTCSection.TEnumerator }

constructor TIPTCSection.TEnumerator.Create(ASection: TIPTCSection);
begin
  FIndex := -1;
  FSource := ASection;
end;

function TIPTCSection.TEnumerator.GetCurrent: TIPTCTag;
begin
  Result := FSource[FIndex];
end;

function TIPTCSection.TEnumerator.MoveNext: Boolean;
begin
  Result := (Succ(FIndex) < FSource.Count);
  if Result then Inc(FIndex);
end;

{ TIPTCSection }

constructor TIPTCSection.Create(AOwner: TIPTCData; AID: TIPTCSectionID);
begin
  inherited Create;
  FOwner := AOwner;
  FID := AID;
  FTags := TObjectList.Create(True);
end;

destructor TIPTCSection.Destroy;
begin
  FTags.Free;
  inherited;
end;

function TIPTCSection.Add(ID: TIPTCTagID): TIPTCTag;
var
  I: Integer;
begin
  if ID = 0 then
    Result := Insert(Count)
  else
  begin
    for I := Count - 1 downto 0 do
      if ID > GetTag(I).ID then
      begin
        Result := Insert(I + 1, ID);
        Exit;
      end;
    Result := Insert(0, ID);
  end;
end;

function TIPTCSection.AddOrUpdate(TagID: TIPTCTagID; NewDataSize: Integer;
  const Buffer): TIPTCTag;
begin
  if not Find(TagID, Result) then
    Result := Add(TagID);
  Result.UpdateData(NewDataSize, Buffer);
end;

function TIPTCSection.Append(ID: TIPTCTagID): TIPTCTag;
begin
  Result := Insert(Count, ID);
end;

function TIPTCSection.Find(ID: TIPTCTagID; out Tag: TIPTCTag): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FTags.Count - 1 do
  begin
    Tag := FTags[I];
    if Tag.ID = ID then Exit;
    if FDefinitelySorted and (Tag.ID > ID) then Break;
  end;
  Tag := nil;
  Result := False;
end;

function TIPTCSection.GetDateValue(TagID: TIPTCTagID): TDateTime;
var
  S: string;
  Year, Month, Day: Integer;
begin
  S := GetStringValue(TagID);
  if not TryStrToInt(Copy(S, 1, 4), Year) or not TryStrToInt(Copy(S, 5, 2), Month) or
    not TryStrToInt(Copy(S, 7, 2), Day) or not TryEncodeDate(Year, Month, Day, Result) then
    Result := 0;
end;

procedure TIPTCSection.SetDateValue(TagID: TIPTCTagID; const Value: TDateTime);
var
  Year, Month, Day: Word;
begin
  if Value = 0 then
  begin
    Remove(TagID);
    Exit;
  end;
  DecodeDate(Value, Year, Month, Day);
  SetStringValue(TagID, Format('%.4d%.2d%.2d', [Year, Month, Day]));
end;

function TIPTCSection.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TIPTCSection.GetStringValue(TagID: TIPTCTagID): string;
var
  Tag: TIPTCTag;
begin
  if Find(TagID, Tag) then
    Result := Tag.ReadString
  else
    Result := '';
end;

function TIPTCSection.Remove(TagID: TIPTCTagID): Integer;
begin
  Result := Remove([TagID]);
end;

function TIPTCSection.Remove(TagIDs: TIPTCTagIDs): Integer;
var
  I: Integer;
  Tag: TIPTCTag;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
  begin
    Tag := Tags[I];
    if Tag.ID in TagIDs then
    begin
      Delete(I);
      Result := I;
    end;
  end;
end;

procedure TIPTCSection.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Delete(I);
end;

procedure TIPTCSection.Delete(Index: Integer);
var
  Tag: TIPTCTag;
begin
  Tag := FTags[Index];
  Tag.FSection := nil;
  FTags.Delete(Index);
  FModified := True;
  if FTags.Count <= 1 then FDefinitelySorted := True;
end;

function TIPTCSection.GetPriorityValue(TagID: TIPTCTagID): TIPTCPriority;
var
  Tag: TIPTCTag;
begin
  if Find(TagID, Tag) and (Tag.DataSize = 1) then
    for Result := Low(TIPTCPriority) to High(TIPTCPriority) do
      if PriorityChars[Result] = PAnsiChar(Tag.Data)^ then Exit;
  Result := ipTagMissing;
end;

function TIPTCSection.GetRepeatableValue(TagID: TIPTCTagID): TStringDynArray;
var
  Count: Integer;
  Tag: TIPTCTag;
begin
  Count := 0;
  Result := nil;
  for Tag in Self do
    if Tag.ID = TagID then
    begin
      if Count = Length(Result) then SetLength(Result, Count + 8);
      Result[Count] := Tag.AsString;
      Inc(Count);
    end;
  if Count <> 0 then SetLength(Result, Count);
end;

procedure TIPTCSection.GetRepeatableValue(TagID: TIPTCTagID; Dest: TStrings;
  ClearDestFirst: Boolean);
var
  Tag: TIPTCTag;
begin
  Dest.BeginUpdate;
  try
    if ClearDestFirst then Dest.Clear;
    for Tag in Self do
      if Tag.ID = TagID then Dest.Add(Tag.AsString);
  finally
    Dest.EndUpdate;
  end;
end;

procedure TIPTCSection.GetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Dest: TStrings;
  ClearDestFirst: Boolean = True);
var
  Keys, Values: TStringDynArray;
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    if ClearDestFirst then Dest.Clear;
    Keys := GetRepeatableValue(KeyID);
    Values := GetRepeatableValue(ValueID);
    if Length(Values) > Length(Keys) then
      SetLength(Keys, Length(Values))
    else
      SetLength(Values, Length(Keys));
    for I := 0 to High(Keys) do
      Dest.Add(Keys[I] + Dest.NameValueSeparator + Values[I]);
  finally
    Dest.EndUpdate;
  end;
end;

procedure TIPTCSection.SetPriorityValue(TagID: TIPTCTagID; Value: TIPTCPriority);
begin
  if Value = ipTagMissing then
    Remove(TagID)
  else
    AddOrUpdate(TagID, 1, PriorityChars[Value]);
end;

procedure TIPTCSection.SetRepeatableValue(TagID: TIPTCTagID; const Value: array of string);
var
  I, DestIndex: Integer;
begin
  DestIndex := Remove(TagID);
  if DestIndex < 0 then
  begin
    DestIndex := 0;
    for I := Count - 1 downto 0 do
      if TagID > GetTag(I).ID then
      begin
        DestIndex := I + 1;
        Break;
      end;
  end;
  for I := 0 to High(Value) do
  begin
    Insert(DestIndex, TagID).AsString := Value[I];
    Inc(DestIndex);
  end;
end;

procedure TIPTCSection.SetRepeatableValue(TagID: TIPTCTagID; Value: TStrings);
var
  DynArray: TStringDynArray;
  I: Integer;
begin
  SetLength(DynArray, Value.Count);
  for I := High(DynArray) downto 0 do
    DynArray[I] := Value[I];
  SetRepeatableValue(TagID, DynArray);
end;

procedure TIPTCSection.SetRepeatablePairs(KeyID, ValueID: TIPTCTagID; Pairs: TStrings);
var
  I, DestIndex: Integer;
  S: string;
begin
  DestIndex := Remove([KeyID, ValueID]);
  if DestIndex < 0 then
  begin
    DestIndex := 0;
    for I := Count - 1 downto 0 do
      if KeyID > GetTag(I).ID then
      begin
        DestIndex := I + 1;
        Break;
      end;
  end;
  for I := 0 to Pairs.Count - 1 do
  begin
    S := Pairs.Names[I];
    if S <> '' then
    begin
      Insert(DestIndex, KeyID).AsString := S;
      Inc(DestIndex);
    end;
    S := Pairs.ValueFromIndex[I];
    if S <> '' then
    begin
      Insert(DestIndex, ValueID).AsString := S;
      Inc(DestIndex);
    end;
  end;
end;

function TIPTCSection.GetTag(Index: Integer): TIPTCTag;
begin
  Result := TIPTCTag(FTags[Index]);
end;

function TIPTCSection.GetTagCount: Integer;
begin
  Result := FTags.Count;
end;

function TIPTCSection.GetWordValue(TagID: TIPTCTagID): TWordTagValue;
var
  Tag: TIPTCTag;
begin
  if Find(TagID, Tag) and (Tag.DataSize = 2) then
    Result := Swap(PWord(Tag.Data)^)
  else
    Result := TWordTagValue.CreateMissingOrInvalid;
end;

function TIPTCSection.Insert(Index: Integer; ID: TIPTCTagID = 0): TIPTCTag;
begin
  if FDefinitelySorted and (Index < Count) and (ID > GetTag(Index).ID) then
    FDefinitelySorted := False;
  Result := TIPTCTag.Create;
  try
    Result.FID := ID;
    Result.FSection := Self;
    FTags.Insert(Index, Result);
  except
    Result.Free;
    raise;
  end;
  FModified := True;
end;

procedure TIPTCSection.Move(CurIndex, NewIndex: Integer);
begin
  if CurIndex = NewIndex then Exit;
  FTags.Move(CurIndex, NewIndex);
  FModified := True;
  FDefinitelySorted := False;
end;

procedure TIPTCSection.SetStringValue(TagID: TIPTCTagID; const Value: string);
var
  Tag: TIPTCTag;
begin
  if Value = '' then
  begin
    Remove(TagID);
    Exit;
  end;
  if not Find(TagID, Tag) then Tag := Add(TagID);
  Tag.WriteString(Value);
end;

procedure TIPTCSection.SetWordValue(TagID: TIPTCTagID; const Value: TWordTagValue);
var
  Data: Word;
begin
  if Value.MissingOrInvalid then
    Remove(TagID)
  else
  begin
    Data := Swap(Value.Value);
    AddOrUpdate(TagID, 2, Data);
  end;
end;

function CompareIDs(Item1, Item2: TIPTCTag): Integer;
begin
  Result := Item2.ID - Item1.ID;
end;

procedure TIPTCSection.Sort;
begin
  if FDefinitelySorted then Exit;
  FTags.Sort(@CompareIDs);
  FModified := True;
  FDefinitelySorted := True;
end;

function TIPTCSection.TagExists(ID: TIPTCTagID; MinSize: Integer = 1): Boolean;
var
  Tag: TIPTCTag;
begin
  Result := Find(ID, Tag) and (Tag.DataSize >= MinSize);
end;

{ TIPTCData.TEnumerator }

constructor TIPTCData.TEnumerator.Create(ASource: TIPTCData);
begin
  FCurrentID := Low(FCurrentID);
  FDoneFirst := False;
  FSource := ASource;
end;

function TIPTCData.TEnumerator.GetCurrent: TIPTCSection;
begin
  Result := FSource.Sections[FCurrentID];
end;

function TIPTCData.TEnumerator.MoveNext: Boolean;
begin
  Result := (FCurrentID < High(FCurrentID));
  if Result and FDoneFirst then
    Inc(FCurrentID)
  else
    FDoneFirst := True;
end;

{ TIPTCData }

constructor TIPTCData.Create;
var
  ID: TIPTCSectionID;
begin
  inherited Create;
  for ID := Low(ID) to High(ID) do
    FSections[ID] := TIPTCSection.Create(Self, ID);
  FUTF8Encoded := True;
end;

destructor TIPTCData.Destroy;
var
  ID: TIPTCSectionID;
begin
  for ID := Low(ID) to High(ID) do
    FSections[ID].Free;
  inherited;
end;

procedure TIPTCData.AddFromStream(Stream: TStream);
var
  MaxHeaderStart: Int64;
  RecID, TagID: Byte;
  DataSize: Int64;
  NewTag: TIPTCTag;
begin
  MaxHeaderStart := Stream.Size - 5;
  while (Stream.Position <= MaxHeaderStart) do
  begin
    if Stream.ReadByte <> NewIPTCTagMarker then
    begin
      Stream.Seek(-1, soCurrent);
      Break;
    end;
    RecID := Stream.ReadByte;
    if (RecID < Low(TIPTCSectionID)) or (RecID > High(TIPTCSectionID)) then
      raise EInvalidIPTCData.CreateFmt(SInvalidIPTCRecordNumber, [RecID]);
    TagID := Stream.ReadByte;
    DataSize := Stream.ReadSmallInt(BigEndian);
    if DataSize < 0 then
      case Abs(DataSize) of
        1: DataSize := Stream.ReadByte;
        2: DataSize := Stream.ReadWord(BigEndian);
        4: DataSize := Stream.ReadLongWord(BigEndian);
      else raise EInvalidIPTCData.CreateFmt(SInvalidIPTCTagSizeField, [Abs(DataSize)]);
      end;
    NewTag := FSections[RecID].Append(TagID);
    try
      NewTag.UpdateData(DataSize, Stream);
    except
      NewTag.Free;
      raise;
    end;
    if (RecID = 1) and (TagID = 90) and (DataSize = SizeOf(UTF8Marker)) and
      CompareMem(NewTag.Data, @UTF8Marker, DataSize) then
      FUTF8Encoded := True;
  end;
end;

procedure TIPTCData.Assign(Source: TPersistent);
var
  SectID: TIPTCSectionID;
  Tag: TIPTCTag;
begin
  if not (Source is TIPTCData) then
  begin
    if Source = nil then
      Clear
    else
      inherited;
    Exit;
  end;
  for SectID := Low(SectID) to High(SectID) do
  begin
    FSections[SectID].Clear;
    for Tag in TIPTCData(Source).Sections[SectID] do
      FSections[SectID].Add(Tag.ID).UpdateData(Tag.DataSize, Tag.Data^);
  end;
end;

procedure TIPTCData.Clear;
var
  ID: TIPTCSectionID;
begin
  for ID := Low(ID) to High(ID) do
    FSections[ID].Clear;
end;

procedure TIPTCData.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', LoadFromStream, SaveToStream, not Empty);
end;

function TIPTCData.CreateIPTCSegment: IJPEGSegment;
begin
  Result := CreateAdobeApp13Segment([CreateAdobeBlock($0404, '', Self)]);
end;

procedure TIPTCData.DoSaveToJPEG(InStream, OutStream: TStream);
var
  Block: IAdobeBlock;
  RewrittenSegment: IJPEGSegment;
  SavedIPTC: Boolean;
  Segment: IFoundJPEGSegment;
  StartPos: Int64;
begin
  SavedIPTC := Empty;
  StartPos := InStream.Position;
  for Segment in JPEGHeader(InStream, [jmApp13]) do
    if Segment.IsAdobeApp13 then
    begin //IrfanView just wipes the original segment and replaces it with the new IPTC data, even if it contained non-IPTC blocks too. Eek!
      InStream.Position := StartPos;
      OutStream.CopyFrom(InStream, Segment.Offset - StartPos);
      if SavedIPTC then
        RewrittenSegment := CreateAdobeApp13Segment
      else
      begin
        RewrittenSegment := CreateIPTCSegment;
        SavedIPTC := True;
      end;
      for Block in Segment do
        if not Block.HasIPTCData then Block.SaveToStream(RewrittenSegment.Data);
      WriteJPEGSegmentToStream(OutStream, RewrittenSegment);
      StartPos := Segment.Offset + Segment.TotalSize;
    end;
  if not SavedIPTC then
  begin
    InStream.Position := StartPos;
    for Segment in JPEGHeader(InStream, AnyJPEGMarker - [jmJFIF, jmApp1]) do
    begin //insert immediately after any Exif or XMP segments if they exist, at the top of the file if they do not
      InStream.Position := StartPos;
      OutStream.CopyFrom(InStream, Segment.Offset - StartPos);
      WriteJPEGSegmentToStream(OutStream, CreateIPTCSegment);
      StartPos := Segment.Offset;
      Break;
    end;
  end;
  InStream.Position := StartPos;
  OutStream.CopyFrom(InStream, InStream.Size - StartPos);
end;

function TIPTCData.GetActionAdvised: TIPTCActionAdvised;
var
  IntValue: Integer;
begin
  if TryStrToInt(ApplicationSection.GetStringValue(itActionAdvised), IntValue) and
    (IntValue > 0) and (IntValue <= 99) then
    Result := TIPTCActionAdvised(IntValue)
  else
    Result := iaTagMissing;
end;

procedure TIPTCData.SetActionAdvised(Value: TIPTCActionAdvised);
begin
  if Value = iaTagMissing then
    ApplicationSection.Remove(itActionAdvised)
  else
    ApplicationSection.SetStringValue(itActionAdvised, Format('%.2d', [Ord(Value)]));
end;

procedure TIPTCData.GetBylineValues(Strings: TStrings);
begin
  ApplicationSection.GetRepeatablePairs(itByline, itBylineTitle, Strings);
end;

procedure TIPTCData.GetContentLocationValues(Strings: TStrings);
begin
  ApplicationSection.GetRepeatablePairs(itContentLocationCode, itContentLocationName, Strings);
end;

function TIPTCData.GetDate(PackedIndex: Integer): TDateTime;
begin
  Result := FSections[Lo(PackedIndex)].GetDateValue(Hi(PackedIndex));
end;

function TIPTCData.GetImageOrientation: TIPTCImageOrientation;
var
  Tag: TIPTCTag;
begin
  if not ApplicationSection.Find(itImageOrientation, Tag) or (Tag.DataSize <> 1) then
    Result := ioTagMissing
  else
    Result := TIPTCImageOrientation(UpCase(PAnsiChar(Tag.Data)^));
end;

procedure TIPTCData.SetImageOrientation(Value: TIPTCImageOrientation);
begin
  if Value = ioTagMissing then
    ApplicationSection.Remove(itImageOrientation)
  else
    ApplicationSection.AddOrUpdate(itImageOrientation, 1, Value);
end;

function TIPTCData.GetApplicationString(TagID: Integer): string;
begin
  Result := ApplicationSection.GetStringValue(TagID)
end;

function TIPTCData.GetApplicationStrings(TagID: Integer): TStringDynArray;
begin
  Result := ApplicationSection.GetRepeatableValue(TagID);
end;

function TIPTCData.GetEnvelopePriority: TIPTCPriority;
begin
  Result := EnvelopeSection.GetPriorityValue(itEnvelopePriority);
end;

function TIPTCData.GetEnvelopeString(TagID: Integer): string;
begin
  Result := EnvelopeSection.GetStringValue(TagID)
end;

function TIPTCData.GetApplicationWord(TagID: Integer): TWordTagValue;
begin
  Result := ApplicationSection.GetWordValue(TagID);
end;

function TIPTCData.GetEnvelopeWord(TagID: Integer): TWordTagValue;
begin
  Result := EnvelopeSection.GetWordValue(TagID);
end;

function TIPTCData.GetUrgency: TIPTCPriority;
begin
  Result := ApplicationSection.GetPriorityValue(itUrgency);
end;

procedure TIPTCData.SetUrgency(Value: TIPTCPriority);
begin
  ApplicationSection.SetPriorityValue(itUrgency, Value);
end;

function TIPTCData.GetEmpty: Boolean;
var
  Section: TIPTCSection;
begin
  Result := False;
  for Section in FSections do
    if Section.Count <> 0 then Exit;
  Result := True;
end;

function TIPTCData.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TIPTCData.GetKeyWords(Dest: TStrings);
begin
  ApplicationSection.GetRepeatableValue(itKeyword, Dest);
end;

function TIPTCData.GetModified: Boolean;
var
  ID: TIPTCSectionID;
begin
  Result := True;
  for ID := Low(ID) to High(ID) do
    if FSections[ID].Modified then Exit;
  Result := False;
end;

function TIPTCData.GetSection(ID: TIPTCSectionID): TIPTCSection;
begin
  Result := FSections[ID];
end;

procedure TIPTCData.LoadFromJPEG(JPEGStream: TStream);
var
  AdobeBlock: IAdobeBlock;
  Segment: IFoundJPEGSegment;
begin
  for Segment in JPEGHeader(JPEGStream, [jmApp13]) do
    for AdobeBlock in Segment do
      if AdobeBlock.HasIPTCData then
      begin
        LoadFromStream(AdobeBlock.Data);
        Exit;
      end;
  Clear;
end;

procedure TIPTCData.LoadFromJPEG(JPEGImage: TJPEGImage);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    JPEGImage.SaveToStream(Stream);
    Stream.Position := 0;
    LoadFromJPEG(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIPTCData.LoadFromJPEG(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromJPEG(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIPTCData.LoadFromStream(Stream: TStream);
var
  I: Integer;
  Section: TIPTCSection;
  WasUTF8Encoded: Boolean;
begin
  Clear;
  WasUTF8Encoded := FUTF8Encoded;
  FUTF8Encoded := AlwaysAssumeUTF8Encoding;
  AddFromStream(Stream);
  if Empty then FUTF8Encoded := WasUTF8Encoded;
  for Section in FSections do
  begin
    Section.Modified := False;
    Section.FDefinitelySorted := True;
    for I := 1 to Section.Count - 1 do
      if Section[I].ID < Section[I - 1].ID then
      begin
        Section.FDefinitelySorted := False;
        Break;
      end;
  end;
end;

procedure TIPTCData.SaveToJPEG(const JPEGFileName: string;
  BufferedWrite: Boolean = True);
var
  InStream: TMemoryStream;
  OutStream: TStream;
begin
  OutStream := nil;
  InStream := TMemoryStream.Create;
  try
    InStream.LoadFromFile(JPEGFileName);
    if BufferedWrite then
      OutStream := TMemoryStream.Create
    else
      OutStream := TFileStream.Create(JPEGFileName, fmCreate);
    DoSaveToJPEG(InStream, OutStream);
    if BufferedWrite then TMemoryStream(OutStream).SaveToFile(JPEGFileName);
  finally
    OutStream.Free;
    InStream.Free;
  end;
end;

procedure TIPTCData.SaveToJPEG(JPEGImage: TJPEGImage);
var
  InStream, OutStream: TMemoryStream;
begin
  OutStream := nil;
  InStream := TMemoryStream.Create;
  try
    JPEGImage.SaveToStream(InStream);
    InStream.Position := 0;
    OutStream := TMemoryStream.Create;
    DoSaveToJPEG(InStream, OutStream);
    OutStream.Position := 0;
    JPEGImage.LoadFromStream(OutStream);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

procedure TIPTCData.SaveToStream(Stream: TStream);
const
  BigDataSizeMarker: SmallInt = -4;
var
  Section: TIPTCSection;
  Tag: TIPTCTag;
begin
  if UTF8Encoded then
    Sections[1].AddOrUpdate(90, SizeOf(UTF8Marker), UTF8Marker)
  else if Sections[1].Find(90, Tag) and (Tag.DataSize = SizeOf(UTF8Marker)) and
    CompareMem(Tag.Data, @UTF8Marker, SizeOf(UTF8Marker)) then Tag.Free;
  for Section in FSections do
    for Tag in Section do
    begin
      Stream.WriteBuffer(NewIPTCTagMarker, SizeOf(NewIPTCTagMarker));
      Stream.WriteBuffer(Tag.Section.ID, SizeOf(TIPTCSectionID));
      Stream.WriteBuffer(Tag.ID, SizeOf(TIPTCTagID));
      if Tag.DataSize <= High(SmallInt) then
        Stream.WriteSmallInt(Tag.DataSize, BigEndian)
      else
      begin
        Stream.WriteSmallInt(BigDataSizeMarker, BigEndian);
        Stream.WriteLongInt(Tag.DataSize, BigEndian);
      end;
      if Tag.DataSize <> 0 then Stream.WriteBuffer(Tag.Data^, Tag.DataSize);
    end;
end;

procedure TIPTCData.SortTags;
var
  SectID: TIPTCSectionID;
begin
  for SectID := Low(SectID) to High(SectID) do
    FSections[SectID].Sort;
end;

procedure TIPTCData.SetBylineValues(Strings: TStrings);
begin
  ApplicationSection.SetRepeatablePairs(itByline, itBylineTitle, Strings);
end;

procedure TIPTCData.SetContentLocationValues(Strings: TStrings);
begin
  ApplicationSection.SetRepeatablePairs(itContentLocationCode, itContentLocationName, Strings);
end;

procedure TIPTCData.SetDate(PackedIndex: Integer; const Value: TDateTime);
begin
  FSections[Lo(PackedIndex)].SetDateValue(Hi(PackedIndex), Value)
end;

procedure TIPTCData.SetApplicationString(TagID: Integer; const Value: string);
begin
  ApplicationSection.SetStringValue(TagID, Value);
end;

procedure TIPTCData.SetApplicationStrings(TagID: Integer; const Value: TStringDynArray);
begin
  ApplicationSection.SetRepeatableValue(TagID, Value);
end;

procedure TIPTCData.SetApplicationWord(TagID: Integer; const Value: TWordTagValue);
begin
  ApplicationSection.SetWordValue(TagID, Value);
end;

procedure TIPTCData.SetEnvelopePriority(const Value: TIPTCPriority);
begin
  EnvelopeSection.SetPriorityValue(itEnvelopePriority, Value);
end;

procedure TIPTCData.SetEnvelopeString(TagID: Integer; const Value: string);
begin
  EnvelopeSection.SetStringValue(TagID, Value);
end;

procedure TIPTCData.SetEnvelopeWord(TagID: Integer; const Value: TWordTagValue);
begin
  EnvelopeSection.SetWordValue(TagID, Value);
end;

procedure TIPTCData.SetKeywords(NewWords: TStrings);
begin
  ApplicationSection.SetRepeatableValue(itKeyword, NewWords);
end;

procedure TIPTCData.SetModified(Value: Boolean);
var
  ID: TIPTCSectionID;
begin
  for ID := Low(ID) to High(ID) do
    FSections[ID].Modified := Value;
end;

procedure TIPTCData.SetUTF8Encoded(Value: Boolean);
var
  I: Integer;
  SectID: TIPTCSectionID;
  Tag: TIPTCTag;
  Strings: array[TIPTCSectionID] of TStringDynArray;
begin
  if Value = FUTF8Encoded then Exit;
  for SectID := Low(SectID) to High(SectID) do
  begin
    SetLength(Strings[SectID], FSections[SectID].Count);
    for I := 0 to High(Strings[SectID]) do
    begin
      Tag := FSections[SectID].Tags[I];
      if FindProperDataType(Tag) = idString then
        Strings[SectID][I] := Tag.ReadString;
    end;
  end;
  FUTF8Encoded := Value;
  for SectID := Low(SectID) to High(SectID) do
    for I := 0 to High(Strings[SectID]) do
    begin
      Tag := FSections[SectID].Tags[I];
      if FindProperDataType(Tag) = idString then
        Tag.WriteString(Strings[SectID][I]);
    end;
end;

end.