{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.2 beta (2010-11-20)                                                      }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.XMPUtils.pas.                                          }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.Exif.XMPUtils;
{
  This unit implements a IDOMDocument/IDOMNode-based XMP packet parser and editor. Since
  v1.1.0, edited data is written out manually (i.e., the IDOMXXX interfaces are only
  used to read). 

  The only slightly funky thing here is the UpdateXXX methods of TXMPPacket, which are
  called automatically when you set a tag property of TCustomExifData. Basically, the
  methods' behaviour depends upon the TXMPPacket's UpdatePolicy property, which has
  three possible values:
  - xwAlwaysUpdate: if the new value is an empty string, then the XMP property is
    deleted, else it is changed (or added). This setting is the default for standalone
    TXMPPacket instances.
  - xwUpdateIfExists: any existing property is updated, but if it doesn't already exist,
    no property is added. This setting is the default for an TXMPPacket instance that is
    attached to a TCustomExifData object; change this to xwAlwaysUpdate to mimic Windows
    Vista's behaviour.
  - xwRemove: always removes the property when UpdateProperty is called.
}
interface

uses
  SysUtils, Classes, xmldom, CCR.Exif.StreamHelper;

{$IF CompilerVersion > 19}
  {$DEFINE DEPCON}
{$IFEND}

type
  EInvalidXMPPacket = class(Exception);
  EInvalidXMPOperation = class(EInvalidOperation);

  TXMPProperty = class;
  TXMPSchema = class;
  TXMPPacket = class;

  TXMPNamespace = (xsUnknown, xsRDF,
    xsCameraRaw, xsColorant, xsDimensions, xsDublinCore,
    xsFont, xsExif, xsExifAux, xsIPTC, xsJob, xsMicrosoftPhoto, xsPDF, xsPhotoshop, xsResourceEvent{*},
    xsResourceRef{*}, xsThumbnail, xsTIFF, xsVersion, xsXMPBasic, xsXMPBasicJobTicket,
    xsXMPDynamicMedia, xsXMPMediaManagement, xsXMPPagedText, xsXMPRights);
  TXMPKnownNamespace = xsRDF..High(TXMPNamespace);

  TKnownXMPNamespaces = record //personally I wouldn't have the 'T', but I'll keep with the D2009+ style...
  private class var
    FKnownURIs: TUnicodeStringList;
    FKnownIndices: array[TXMPKnownNamespace] of Integer;
    class procedure NeedKnownURIs; static;
    class function GetPreferredPrefix(Namespace: TXMPKnownNamespace): UnicodeString; static;
    class function GetURI(Namespace: TXMPKnownNamespace): UnicodeString; static;
  public
    class function Find(const URI: UnicodeString; out Namespace: TXMPNamespace): Boolean; static;
    class property PreferredPrefix[Namespace: TXMPKnownNamespace]: UnicodeString read GetPreferredPrefix;
    class property URI[Namespace: TXMPKnownNamespace]: UnicodeString read GetURI;
  end;

  TXMPNamespaceInfo = class(TPersistent)
  strict private
    FCustomPrefix: UnicodeString;
    FCustomURI: UnicodeString;
    FKind: TXMPNamespace;
    FOnChange: TNotifyEvent;
    procedure Changed;
    function GetPrefix: UnicodeString;
    function GetURI: UnicodeString;
    procedure SetPrefix(const Value: UnicodeString);
  protected
    constructor Create(AKind: TXMPNamespace;
      const APreferredPrefix, AURI: UnicodeString); overload;
    procedure DoAssign(Source: TXMPNamespaceInfo); overload;
    function DoAssign(const APrefix, AURI: UnicodeString): Boolean; overload;
  public
    constructor Create(AKind: TXMPKnownNamespace); overload;
    constructor Create(const APreferredPrefix, AURI: UnicodeString); overload;
    constructor Create(ASource: TXMPNamespaceInfo); overload;
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(AKind: TXMPKnownNamespace); reintroduce; overload;
    procedure Assign(const APrefix, AURI: UnicodeString); reintroduce; overload; 
    property Kind: TXMPNamespace read FKind;
    property Prefix: UnicodeString read GetPrefix write SetPrefix;
    property URI: UnicodeString read GetURI;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  IXMPPropertyEnumerator = interface
  ['{32054DDD-5415-4F5D-8A38-C79BBCFD1A50}']
    function GetCurrent: TXMPProperty;
    function MoveNext: Boolean;
    property Current: TXMPProperty read GetCurrent;
  end;

  IXMPPropertyCollection = interface
  ['{A72E8B43-34AE-49E8-A889-36B51B6A6E48}']
    function GetNamespaceInfo: TXMPNamespaceInfo;
    function GetProperty(Index: Integer): TXMPProperty;
    function GetPropertyCount: Integer;
    function GetEnumerator: IXMPPropertyEnumerator;
    property Count: Integer read GetPropertyCount;
    property Items[Index: Integer]: TXMPProperty read GetProperty; default;
    property NamespaceInfo: TXMPNamespaceInfo read GetNamespaceInfo;
  end;

  TXMPPropertyKind = (xpSimple, xpStructure, xpAltArray, xpBagArray, xpSeqArray);

  TXMPProperty = class(TInterfacedPersistent, IXMPPropertyCollection)
  strict private
    FKind: TXMPPropertyKind;
    FName: UnicodeString;
    FNamespaceInfo: TXMPNamespaceInfo;
    FParentNamespace: Boolean;
    FParentProperty: TXMPProperty;
    FSchema: TXMPSchema;
    FSubProperties: TList;
    FValue: UnicodeString;
    procedure NamespaceInfoChanged(Sender: TObject);
    procedure SetKind(const Value: TXMPPropertyKind);
    procedure SetName(const Value: UnicodeString);
    procedure SetParentNamespace(Value: Boolean);
    procedure SetSubPropertyCount(NewCount: Integer);
  protected
    procedure Changed;
    function GetNamespaceInfo: TXMPNamespaceInfo;
    function GetSubProperty(Index: Integer): TXMPProperty;
    function GetSubPropertyByName(const AName: UnicodeString): TXMPProperty;
    function GetSubPropertyCount: Integer;
    function IXMPPropertyCollection.GetProperty = GetSubProperty;
    function IXMPPropertyCollection.GetPropertyCount = GetSubPropertyCount;
  public
    class function HasNamedSubProperties(Kind: TXMPPropertyKind): Boolean; overload; static; inline;
    class function SupportsSubProperties(Kind: TXMPPropertyKind): Boolean; overload; static; inline;
  public
    constructor Create(ASchema: TXMPSchema; AParentProperty: TXMPProperty;
      const ASourceNode: IDOMNode = nil);
    destructor Destroy; override;
    function GetEnumerator: IXMPPropertyEnumerator;
    function AddSubProperty(const AName: UnicodeString): TXMPProperty; //AName is ignored if self is an array property
    function FindSubProperty(const AName: UnicodeString; out Prop: TXMPProperty): Boolean;
    function ReadValue(const Default: Boolean): Boolean; overload;
    function ReadValue(const Default: Integer): Integer; overload;
    function ReadValue(const Default: UnicodeString = ''): UnicodeString; overload;
    function RemoveSubProperty(const AName: UnicodeString): Boolean;
    function HasNamedSubProperties: Boolean; overload;
    function SupportsSubProperties: Boolean; overload;
    procedure UpdateSubProperty(const SubPropName: UnicodeString;
      SubPropKind: TXMPPropertyKind; const NewValue: UnicodeString); overload;
    procedure UpdateSubProperty(const SubPropName, NewValue: UnicodeString); overload; inline;
    procedure UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Integer); overload;
    procedure UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Boolean); overload;
    procedure WriteValue(const NewValue: UnicodeString); overload;
    procedure WriteValue(const NewValue: Integer); overload;
    procedure WriteValue(const NewValue: Boolean); overload;
    property Kind: TXMPPropertyKind read FKind write SetKind;
    property Name: UnicodeString read FName write SetName;
    property NamespaceInfo: TXMPNamespaceInfo read FNamespaceInfo;
    property ParentNamespace: Boolean read FParentNameSpace write SetParentNamespace default True;
    property ParentProperty: TXMPProperty read FParentProperty;
    property Schema: TXMPSchema read FSchema;
    property SubProperties[const Name: UnicodeString]: TXMPProperty read GetSubPropertyByName; default; //adds if necessary
    property SubProperties[Index: Integer]: TXMPProperty read GetSubProperty; default;
    property SubPropertyCount: Integer read GetSubPropertyCount write SetSubPropertyCount;
  end;

  TXMPSchemaKind = TXMPNamespace; //will be deprecated for what it's now aliased to
  TXMPKnownSchemaKind = TXMPKnownNamespace; //ditto
  TXMPKnownSchemaKinds = set of TXMPKnownNamespace;

  TXMPSchema = class(TInterfacedPersistent, IXMPPropertyCollection)
  strict private
    FLoadingProperty: Boolean;
    FNamespaceInfo: TXMPNamespaceInfo;
    FOwner: TXMPPacket;
    FProperties: TList;
    procedure NamespaceInfoChanged(Sender: TObject);
  protected
    function LoadProperty(const ASourceNode: IDOMNode): TXMPProperty;
    procedure Changed;
    function FindOrAddProperty(const AName: UnicodeString): TXMPProperty;
    function GetNamespaceInfo: TXMPNamespaceInfo;
    function GetOwner: TPersistent; override;
    function GetProperty(Index: Integer): TXMPProperty;
    function GetPropertyCount: Integer;
  public //deprecated - to be removed in a later release
    function Kind: TXMPNamespace; deprecated {$IFDEF DEPCON}'Use NamespaceInfo.Kind'{$ENDIF};
    function PreferredPrefix: UnicodeString; deprecated {$IFDEF DEPCON}'Use NamespaceInfo.Prefix'{$ENDIF};
    function URI: UnicodeString; deprecated {$IFDEF DEPCON}'Use NamespaceInfo.URI'{$ENDIF};
  public
    constructor Create(AOwner: TXMPPacket; const AURI: UnicodeString);
    destructor Destroy; override;
    function GetEnumerator: IXMPPropertyEnumerator;
    function AddProperty(const AName: UnicodeString): TXMPProperty; 
    function FindProperty(const AName: UnicodeString; var AProperty: TXMPProperty): Boolean;
    function RemoveProperty(const AName: UnicodeString): Boolean;
    function RemoveProperties(const ANames: array of UnicodeString): Boolean;
    property NamespaceInfo: TXMPNamespaceInfo read FNamespaceInfo;
    property Owner: TXMPPacket read FOwner;
    property Properties[const Name: UnicodeString]: TXMPProperty read FindOrAddProperty; default;
    property Properties[Index: Integer]: TXMPProperty read GetProperty; default;
    property PropertyCount: Integer read GetPropertyCount;
  end;

  TXMPWritePolicy = (xwAlwaysUpdate, xwUpdateIfExists, xwRemove);

  TXMPPacket = class(TInterfacedPersistent, IStreamPersist)
  public type
    TEnumerator = record
    private
      FIndex: Integer;
      FPacket: TXMPPacket;
      function GetCurrent: TXMPSchema;
    public
      constructor Create(Packet: TXMPPacket);
      function MoveNext: Boolean;
      property Current: TXMPSchema read GetCurrent;
    end;
    TLoadErrorEvent = procedure (Sender: TXMPPacket; Source: TStream) of object;
  strict private
    FAboutAttributeValue: UnicodeString;
    FRawXMLCache: UTF8String;
    FSchemas: TUnicodeStringList;
    FUpdatePolicy: TXMPWritePolicy;
    FWriteSegmentHeader: Boolean;
    FOnChange: TNotifyEvent;
    FOnLoadError: TLoadErrorEvent;
    procedure Clear(StillUpdating: Boolean); overload;
    function GetEmpty: Boolean;
    function GetRawXML: UTF8String;
    function GetSchema(Index: Integer): TXMPSchema;
    function GetSchemaCount: Integer;
    procedure SetAboutAttributeValue(const Value: UnicodeString);
    procedure SetRawXML(const XML: UTF8String);
    procedure DoUpdateProperty(Policy: TXMPWritePolicy; SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; PropKind: TXMPPropertyKind; const NewValue: UnicodeString);
    procedure DoUpdateArrayProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; ArrayPropKind: TXMPPropertyKind;
      const NewValues: array of UnicodeString); overload;
  protected
    procedure Changed(ResetRawXMLCache: Boolean = True); virtual;
    function FindOrAddSchema(const URI: UnicodeString): TXMPSchema; overload;
    function FindOrAddSchema(Kind: TXMPKnownNamespace): TXMPSchema; overload; inline;
    procedure LoadError(Source: TStream); virtual;
    property UpdatePolicy: TXMPWritePolicy read FUpdatePolicy write FUpdatePolicy;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; overload;
    function FindSchema(const URI: UnicodeString; var Schema: TXMPSchema): Boolean; overload;
    function FindSchema(Kind: TXMPKnownNamespace; var Schema: TXMPSchema): Boolean; overload;
    function GetEnumerator: TEnumerator;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    function LoadFromJPEG(const JPEGFileName: string): Boolean;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function TryLoadFromStream(Stream: TStream): Boolean;
    procedure RemoveProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString); overload;
    procedure RemoveProperties(SchemaKind: TXMPKnownNamespace;
      const PropNames: array of UnicodeString); overload;
    procedure UpdateProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      PropKind: TXMPPropertyKind; const NewValue: UnicodeString); overload;
    procedure UpdateBagProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValues: array of UnicodeString); overload;
    procedure UpdateBagProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValueList: UnicodeString); overload; inline; //commas and/or semi-colons act as delimiters
    procedure UpdateSeqProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValues: array of UnicodeString); overload;
    procedure UpdateSeqProperty(SchemaKind: TXMPKnownNamespace; const PropName: UnicodeString;
      const NewValueList: UnicodeString); overload; inline; //commas and/or semi-colons act as delimiters
    procedure UpdateProperty(SchemaKind: TXMPKnownNamespace;
      const PropName, NewValue: UnicodeString); overload; inline;
    procedure UpdateProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; const NewValue: Integer); overload;
    procedure UpdateDateTimeProperty(SchemaKind: TXMPKnownNamespace;
      const PropName: UnicodeString; const NewValue: TDateTime; ApplyLocalBias: Boolean = False); overload;
    property AboutAttributeValue: UnicodeString read FAboutAttributeValue write SetAboutAttributeValue;
    property Empty: Boolean read GetEmpty;
    property RawXML: UTF8String read GetRawXML write SetRawXML;
    property SchemaCount: Integer read GetSchemaCount;
    property Schemas[Kind: TXMPKnownNamespace]: TXMPSchema read FindOrAddSchema; default;
    property Schemas[Index: Integer]: TXMPSchema read GetSchema; default;
    property Schemas[const URI: UnicodeString]: TXMPSchema read FindOrAddSchema; default;
    property WriteSegmentHeader: Boolean read FWriteSegmentHeader write FWriteSegmentHeader;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLoadError: TLoadErrorEvent read FOnLoadError write FOnLoadError;
  end;

const
  XMPBoolStrs: array[Boolean] of string = ('False', 'True'); //case as per the XMP spec
  XMPSegmentHeader: array[0..28] of AnsiChar = 'http://ns.adobe.com/xap/1.0/'#0;

function DateTimeToXMPString(Value: TDateTime; ApplyLocalBias: Boolean): UnicodeString;
function EscapeXML(const Source: UnicodeString): UnicodeString;
function HasXMPSegmentHeader(Stream: TStream): Boolean;
function UTF8ToString(const UTF8: UTF8String): string; inline; overload;
function UTF8ToString(UTF8Buffer: Pointer; ByteLen: Integer): string; overload;

implementation

uses
  Math, RTLConsts, Contnrs, StrUtils, XSBuiltIns, CCR.Exif.Consts, CCR.Exif.JPEGUtils; //XSBuiltIns for DateTimeToXMLTime

function DateTimeToXMPString(Value: TDateTime; ApplyLocalBias: Boolean): UnicodeString;
begin
  Result := DateTimeToXMLTime(Value, ApplyLocalBias)
end;

function DefinesNS(const Attr: IDOMNode): Boolean;
begin
  Result := (Attr.prefix = 'xmlns') or (Attr.namespaceURI = 'http://www.w3.org/2000/xmlns/');
end;

function EscapeXML(const Source: UnicodeString): UnicodeString;
var
  Ch: WideChar;
begin
  with TMemoryStream.Create do
  try
    for Ch in Source do
      case Ch of
        '<': WriteWideChars('&lt;', SmallEndian);
        '>': WriteWideChars('&gt;', SmallEndian);
        '&': WriteWideChars('&amp;', SmallEndian);
        '''': WriteWideChars('&apos;', SmallEndian);
        '"': WriteWideChars('&quot;', SmallEndian);
      else WriteBuffer(Ch, 2);
      end;
    SetString(Result, PWideChar(Memory), Size div 2);
  finally
    Free;
  end;
end;
function FindRootRDFNode(const Document: IDOMDocument; out Node: IDOMNode): Boolean;
begin
  Result := True;
  Node := Document.firstChild;
  while Node <> nil do
  begin
    if Node.nodeType = ELEMENT_NODE then
      case IndexStr(Node.localName, ['RDF', 'xmpmeta', 'xapmeta']) of
        0: Exit; //support ExifTool's XML dumps, which don't parent the RDF node
        1..2: Break;
      end;
    Node := Node.nextSibling;
  end;
  if Node <> nil then
  begin
    Node := Node.firstChild;
    while Node <> nil do
    begin
      if (Node.nodeName = 'rdf:RDF') and (Node.nodeType = ELEMENT_NODE) then Break;
      Node := Node.nextSibling;
    end;
  end;
  Result := (Node <> nil);
end;

function HasXMPSegmentHeader(Stream: TStream): Boolean;
begin
  Result := Stream.TryReadHeader(XMPSegmentHeader, SizeOf(XMPSegmentHeader));
  if Result then Stream.Seek(-SizeOf(XMPSegmentHeader), soCurrent);
end;

procedure UpdateChildNamespaceInfos(const Parent: IXMPPropertyCollection);
var
  Prop: TXMPProperty;
begin
  for Prop in Parent do
    if Prop.ParentNamespace then
    begin
      Prop.NamespaceInfo.DoAssign(Parent.NamespaceInfo);
      UpdateChildNamespaceInfos(Prop);
    end;
end;

function UTF8ToString(const UTF8: UTF8String): string;
begin
  {$IFDEF UNICODE}
  Result := string(UTF8);
  {$ELSE}
  Result := Utf8ToAnsi(UTF8);
  {$ENDIF}
end;

function UTF8ToString(UTF8Buffer: Pointer; ByteLen: Integer): string;
var
  S: UTF8String;
begin
  SetString(S, PAnsiChar(UTF8Buffer), ByteLen);
  Result := UTF8ToString(S);
end;

const
  XMLLangAttrName = 'xml:lang';
  DefaultLangIdent = 'x-default';

type
  RDF = record const
    URI = UnicodeString('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    PreferredPrefix = 'rdf';
    AboutAttrLocalName = 'about';
    AboutAttrName = PreferredPrefix + ':' + AboutAttrLocalName;
    AltNodeName = PreferredPrefix + ':' + 'Alt';
    BagNodeName = PreferredPrefix + ':' + 'Bag';
    DescriptionNodeName = PreferredPrefix + ':' + 'Description';
    ListNodeLocalName = 'li';
    ListNodeName = PreferredPrefix + ':' + ListNodeLocalName;
    SeqNodeName = PreferredPrefix + ':' + 'Seq';
  end;

  TStringListThatOwnsItsObjects = class(TUnicodeStringList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

  TXMPPropertyEnumerator = class(TInterfacedObject, IXMPPropertyEnumerator)
  strict private
    FIndex: Integer;
    FSource: TList;
  protected
    function GetCurrent: TXMPProperty;
    function MoveNext: Boolean;
  public
    constructor Create(Source: TList);
  end;

{ TKnownXMPNamespaces }

class procedure TKnownXMPNamespaces.NeedKnownURIs;
var
  I: Integer;
begin
  if FKnownURIs <> nil then Exit;
  FKnownURIs := TUnicodeStringList.Create;
  with FKnownURIs do
  begin
    AddObject(RDF.URI, TObject(xsRDF));
    AddObject('http://ns.adobe.com/camera-raw-settings/1.0/', TObject(xsCameraRaw));
    AddObject('http://ns.adobe.com/xap/1.0/g', TObject(xsColorant));
    AddObject('http://ns.adobe.com/xap/1.0/sType/Dimensions#', TObject(xsDimensions));
    AddObject('http://purl.org/dc/elements/1.1/', TObject(xsDublinCore));
    AddObject('http://ns.adobe.com/xap/1.0/sType/Font#', TObject(xsFont));
    AddObject('http://ns.adobe.com/exif/1.0/', TObject(xsExif));
    AddObject('http://ns.adobe.com/exif/1.0/aux/', TObject(xsExifAux));
    AddObject('http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/', TObject(xsIPTC));
    AddObject('http://ns.adobe.com/xap/1.0/sType/Job#', TObject(xsJob));
    AddObject('http://ns.microsoft.com/photo/1.0', TObject(xsMicrosoftPhoto));
    AddObject('http://ns.adobe.com/pdf/1.3/', TObject(xsPDF));
    AddObject('http://ns.adobe.com/photoshop/1.0/', TObject(xsPhotoshop));
    AddObject('http://ns.adobe.com/xap/1.0/sType/ResourceEvent#', TObject(xsResourceEvent));
    AddObject('http://ns.adobe.com/xap/1.0/sType/ResourceRef#', TObject(xsResourceRef));
    AddObject('http://ns.adobe.com/xap/1.0/g/img/', TObject(xsThumbnail));
    AddObject('http://ns.adobe.com/tiff/1.0/', TObject(xsTIFF));
    AddObject('http://ns.adobe.com/xap/1.0/sType/Version#', TObject(xsVersion));
    AddObject('http://ns.adobe.com/xap/1.0/', TObject(xsXMPBasic));
    AddObject('http://ns.adobe.com/xap/1.0/bj/', TObject(xsXMPBasicJobTicket));
    AddObject('http://ns.adobe.com/xmp/1.0/DynamicMedia/', TObject(xsXMPDynamicMedia));
    AddObject('http://ns.adobe.com/xap/1.0/mm/', TObject(xsXMPMediaManagement));
    AddObject('http://ns.adobe.com/xap/1.0/t/pg/', TObject(xsXMPPagedText));
    AddObject('http://ns.adobe.com/xap/1.0/rights/', TObject(xsXMPRights));
    Sorted := True;
    for I := Count - 1 downto 0 do
      FKnownIndices[TXMPKnownNamespace(Objects[I])] := I;
  end;
end;

class function TKnownXMPNamespaces.GetPreferredPrefix(Namespace: TXMPKnownNamespace): UnicodeString;
begin
  case Namespace of
    xsRDF: Result := RDF.PreferredPrefix;
    xsCameraRaw: Result := 'crs';
    xsColorant: Result := 'xmpG';
    xsDimensions: Result := 'stDim';
    xsDublinCore: Result := 'dc';
    xsFont: Result := 'stFnt';
    xsExif: Result := 'exif';
    xsExifAux: Result := 'aux';
    xsIPTC: Result := 'Iptc4xmpCore';
    xsJob: Result := 'stJob';
    xsMicrosoftPhoto: Result := 'MicrosoftPhoto';
    xsPDF: Result := 'pdf';
    xsPhotoshop: Result := 'photoshop';
    xsResourceEvent: Result := 'stEvt';
    xsResourceRef: Result := 'stRef';
    xsThumbnail: Result := 'xmpGImg';
    xsTIFF: Result := 'tiff';
    xsVersion: Result := 'stVer';
    xsXMPBasic: Result := 'xmp';
    xsXMPBasicJobTicket: Result := 'xmpBJ';
    xsXMPDynamicMedia: Result := 'xmpDM';
    xsXMPMediaManagement: Result := 'xmpMM';
    xsXMPPagedText: Result := 'xmpTPg';
    xsXMPRights: Result := 'xmpRights';
  else Assert(False);
  end;
end;

class function TKnownXMPNamespaces.GetURI(Namespace: TXMPKnownNamespace): UnicodeString;
begin
  NeedKnownURIs;
  Result := FKnownURIs[FKnownIndices[Namespace]];
end;

class function TKnownXMPNamespaces.Find(const URI: UnicodeString; out Namespace: TXMPNamespace): Boolean;
var
  Index: Integer;
begin
  NeedKnownURIs;
  Result := FKnownURIs.Find(URI, Index);
  if Result then
    Namespace := TXMPNamespace(FKnownURIs.Objects[Index])
  else
    Namespace := xsUnknown;
end;

destructor TStringListThatOwnsItsObjects.Destroy;
begin
  Clear;
  inherited;
end;

procedure TStringListThatOwnsItsObjects.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Objects[I].Free;
  inherited;
end;

procedure TStringListThatOwnsItsObjects.Delete(Index: Integer);
begin
  Objects[Index].Free;
  inherited;
end;

{ TXMPPropertyEnumerator }

constructor TXMPPropertyEnumerator.Create(Source: TList);
begin
  FIndex := -1;
  FSource := Source;
end;

function TXMPPropertyEnumerator.GetCurrent: TXMPProperty;
begin
  Result := FSource[FIndex];
end;

function TXMPPropertyEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FIndex < FSource.Count);
end;

{ TXMPNamespaceInfo }

constructor TXMPNamespaceInfo.Create(AKind: TXMPNamespace;
  const APreferredPrefix, AURI: UnicodeString);
begin
  inherited Create;
  FKind := AKind;
  FCustomPrefix := APreferredPrefix;
  FCustomURI := AURI;
end;

constructor TXMPNamespaceInfo.Create(AKind: TXMPKnownNamespace);
begin
  Create(AKind, '', '');
end;

constructor TXMPNamespaceInfo.Create(const APreferredPrefix, AURI: UnicodeString);
begin
  Create(xsUnknown, APreferredPrefix, AURI);
end;

constructor TXMPNamespaceInfo.Create(ASource: TXMPNamespaceInfo);
begin
  inherited Create;
  Assign(ASource);
end;

procedure TXMPNamespaceInfo.Assign(Source: TPersistent);
begin
  if Source is TXMPNamespaceInfo then
  begin
    DoAssign(TXMPNamespaceInfo(Source));
    Changed;
    Exit;
  end;
  inherited;
end;

procedure TXMPNamespaceInfo.Assign(AKind: TXMPKnownNamespace);
begin
  if FKind = AKind then Exit;
  FCustomPrefix := '';
  FCustomURI := '';
  FKind := AKind;
  Changed;
end;

procedure TXMPNamespaceInfo.Assign(const APrefix, AURI: UnicodeString);
begin
  if DoAssign(APrefix, AURI) then Changed;
end;

procedure TXMPNamespaceInfo.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMPNamespaceInfo.DoAssign(Source: TXMPNamespaceInfo);
begin
  FCustomPrefix := Source.FCustomPrefix;
  FCustomURI := Source.FCustomURI;
  FKind := Source.FKind;
end;

function TXMPNamespaceInfo.DoAssign(const APrefix, AURI: UnicodeString): Boolean;
begin
  Result := (AURI <> URI) or (APrefix <> Prefix);
  if not Result then Exit;
  if TKnownXMPNamespaces.Find(AURI, FKind) and (AURI = TKnownXMPNamespaces.PreferredPrefix[FKind]) then
  begin
    FCustomPrefix := '';
    FCustomURI := '';
  end
  else
  begin
    FCustomPrefix := APrefix;
    FCustomURI := AURI;
  end;
end;

function TXMPNamespaceInfo.GetPrefix: UnicodeString;
begin
  if (FCustomPrefix <> '') or (FKind = xsUnknown) then
    Result := FCustomPrefix
  else
    Result := TKnownXMPNamespaces.PreferredPrefix[FKind];
end;

function TXMPNamespaceInfo.GetURI: UnicodeString;
begin
  if FKind = xsUnknown then
    Result := FCustomURI
  else
    Result := TKnownXMPNamespaces.URI[FKind];
end;

procedure TXMPNamespaceInfo.SetPrefix(const Value: UnicodeString);
begin
  if Value = Prefix then Exit;
  FCustomPrefix := Value;
  Changed;
end;

{ TXMPProperty }

class function TXMPProperty.HasNamedSubProperties(Kind: TXMPPropertyKind): Boolean;
begin
  Result := (Kind in [xpStructure, xpAltArray]);
end;

class function TXMPProperty.SupportsSubProperties(Kind: TXMPPropertyKind): Boolean;
begin
  Result := (Kind in [xpAltArray, xpBagArray, xpSeqArray, xpStructure]);
end;

constructor TXMPProperty.Create(ASchema: TXMPSchema; AParentProperty: TXMPProperty;
  const ASourceNode: IDOMNode);

  procedure UseSourceNodeNameAndNamespace;
  begin
    FName := ASourceNode.localName;
    if FNamespaceInfo.DoAssign(ASourceNode.prefix, ASourceNode.namespaceURI) then
      FParentNamespace := False;
  end;
var
  Attrs: IDOMNamedNodeMap;
  ChildNode, DataNode: IDOMNode;
  DoAdd: Boolean;
  I: Integer;
  PropAndNodeStructure: Boolean;
  S: string;
  SourceAsElem: IDOMElement;
begin
  inherited Create;
  FParentNamespace := True;
  FNamespaceInfo := TXMPNamespaceInfo.Create(ASchema.NamespaceInfo);
  if AParentProperty <> nil then
    FNamespaceInfo.DoAssign(AParentProperty.NamespaceInfo)
  else if ASchema <> nil then
    FNamespaceInfo.DoAssign(ASchema.NamespaceInfo)
  else
    FParentNamespace := False;
  FParentProperty := AParentProperty;
  FSchema := ASchema;
  FSubProperties := TObjectList.Create;
  if ASourceNode = nil then Exit;
  //figure out our kind
  PropAndNodeStructure := Supports(ASourceNode, IDOMElement, SourceAsElem) and
    (SourceAsElem.getAttributeNS(RDF.URI, 'parseType') = 'Resource'); //http://www.w3.org/TR/REC-rdf-syntax/#section-Syntax-parsetype-resource
  if PropAndNodeStructure then
  begin
    FKind := xpStructure;
    DataNode := ASourceNode;
  end
  else
  begin
    DataNode := ASourceNode.firstChild;
    if DataNode <> nil then
      repeat
        case DataNode.nodeType of
          TEXT_NODE: Break;
          ELEMENT_NODE:
          begin
            S := DataNode.nodeName;
            if S = RDF.AltNodeName then
              FKind := xpAltArray
            else if S = RDF.BagNodeName then
              FKind := xpBagArray
            else if S = RDF.SeqNodeName then
              FKind := xpSeqArray
            else if S = RDF.DescriptionNodeName then
              FKind := xpStructure;
            Break;
          end;
        end;
        DataNode := DataNode.nextSibling;
      until (DataNode = nil);
  end;
  { Get the value, if appropriate, and check for super-inlined structures. For life of
    me I can't see where in the XMP spec the latter are allowed - cf. p.19 of the spec
    pt.1 PDF), but Adobe's XMP Toolkit can output them, at least in v4.x. }
  if FKind = xpSimple then
    if DataNode <> nil then
      FValue := DataNode.nodeValue
    else if ASourceNode.attributes <> nil then
    begin
      Attrs := ASourceNode.attributes;
      for I := 0 to Attrs.length - 1 do
        if not DefinesNS(Attrs[I]) then
        begin
          DataNode := ASourceNode;
          FKind := xpStructure;
          Break;
        end;
    end;
  //get the name
  if ParentProperty = nil then
    UseSourceNodeNameAndNamespace
  else
    case ParentProperty.Kind of
      xpBagArray, xpSeqArray: FName := '';
      xpAltArray:
        if SourceAsElem <> nil then
          FName := SourceAsElem.getAttribute(XMLLangAttrName)
        else
          FName := '';
    else UseSourceNodeNameAndNamespace;
    end;
  //load any sub-props
  if not SupportsSubProperties then Exit;
  Assert(DataNode <> nil);
  Attrs := DataNode.attributes;
  if (FKind = xpStructure) and not PropAndNodeStructure and (Attrs <> nil) then
  begin
    FSubProperties.Capacity := Attrs.length;
    for I := 0 to FSubProperties.Capacity - 1 do
    begin
      ChildNode := Attrs[I];
      if not DefinesNS(ChildNode) then
        FSubProperties.Add(TXMPProperty.Create(Schema, Self, ChildNode));
    end;
  end;
  FSubProperties.Capacity := FSubProperties.Count + DataNode.childNodes.length;
  ChildNode := DataNode.firstChild;
  while ChildNode <> nil do
  begin
    DoAdd := False;
    if ChildNode.nodeType = ELEMENT_NODE then
    begin
      if FKind = xpStructure then
        DoAdd := True
      else if ChildNode.nodeName = RDF.ListNodeName then
        DoAdd := (FKind <> xpAltArray) or
          (ChildNode.attributes.getNamedItem(XMLLangAttrName) <> nil);
    end;
    if DoAdd then
      FSubProperties.Add(TXMPProperty.Create(Schema, Self, ChildNode));
    ChildNode := ChildNode.nextSibling;
  end;
  FNamespaceInfo.OnChange := NamespaceInfoChanged;
end;

destructor TXMPProperty.Destroy;
begin
  FNamespaceInfo.Free;
  FSubProperties.Free;
  inherited;
end;

function TXMPProperty.AddSubProperty(const AName: UnicodeString): TXMPProperty;
begin
  if not SupportsSubProperties then
    raise EInvalidXMPOperation.CreateRes(@SSubPropertiesNotSupported);
  if HasNamedSubProperties and (AName = '') then
    raise EInvalidXMPOperation.CreateRes(@SSubPropertiesMustBeNamed);
  Result := TXMPProperty.Create(Schema, Self);
  FSubProperties.Add(Result);
  if HasNamedSubProperties then Result.FName := AName;
  Changed;
end;

procedure TXMPProperty.Changed;
begin
  if FSchema <> nil then FSchema.Changed;
end;

function TXMPProperty.FindSubProperty(const AName: UnicodeString; out Prop: TXMPProperty): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := SubPropertyCount - 1 downto 0 do
  begin
    Prop := SubProperties[I];
    if Prop.Name = AName then Exit;
  end;
  Prop := nil;
  Result := False;
end;

function TXMPProperty.GetEnumerator: IXMPPropertyEnumerator;
begin
  Result := TXMPPropertyEnumerator.Create(FSubProperties);
end;

function TXMPProperty.GetNamespaceInfo: TXMPNamespaceInfo;
begin
  Result := FNamespaceInfo;
end;

function TXMPProperty.GetSubProperty(Index: Integer): TXMPProperty;
begin
  Result := TXMPProperty(FSubProperties[Index]);
end;

function TXMPProperty.GetSubPropertyByName(const AName: UnicodeString): TXMPProperty;
begin
  if not FindSubProperty(AName, Result) then
    Result := AddSubProperty(AName);
end;

function TXMPProperty.GetSubPropertyCount: Integer;
begin
  Result := FSubProperties.Count;
end;

procedure TXMPProperty.NamespaceInfoChanged(Sender: TObject);
begin
  FParentNamespace := False;
  UpdateChildNamespaceInfos(Self);
  Changed;
end;

function TXMPProperty.ReadValue(const Default: Boolean): Boolean;
var
  S: string;
begin
  S := ReadValue;
  if SameText(S, XMPBoolStrs[True]) then
    Result := True
  else if SameText(S, XMPBoolStrs[False]) then
    Result := False
  else if S = '1' then
    Result := True
  else if S = '0' then
    Result := False
  else
    Result := Default;
end;

function TXMPProperty.ReadValue(const Default: Integer): Integer;
begin
  Result := StrToIntDef(ReadValue, Default);
end;

function TXMPProperty.ReadValue(const Default: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  case Kind of
    xpSimple: if Pointer(FValue) <> nil then Result := FValue else Result := Default;
    xpAltArray: Result := SubProperties[DefaultLangIdent].ReadValue(Default);
    xpBagArray, xpSeqArray:
      case SubPropertyCount of
        0: Result := Default;
        1: Result := SubProperties[0].ReadValue(Default);
      else
        Result := SubProperties[0].ReadValue;
        for I := 1 to SubPropertyCount - 1 do
          Result := Result + ',' + SubProperties[I].ReadValue;
      end;
  else Result := Default;
  end;
end;

function TXMPProperty.RemoveSubProperty(const AName: UnicodeString): Boolean;
var
  I: Integer;
  Prop: TXMPProperty;
begin
  Result := False;
  for I := 0 to SubPropertyCount - 1 do
  begin
    Prop := FSubProperties.List[I];
    if AName = Prop.Name then
    begin
      FSubProperties.Delete(I);
      Result := True;
      Changed;
      Exit;
    end;
  end;
end;

procedure TXMPProperty.SetKind(const Value: TXMPPropertyKind);
var
  I: Integer;
  SubProp: TXMPProperty;
begin
  if Value = Kind then Exit;
  if not SupportsSubProperties(Value) then
    SubPropertyCount := 0
  else
    for I := SubPropertyCount - 1 downto 0 do
    begin
      SubProp := SubProperties[I];
      if not HasNamedSubProperties(Value) then
        SubProp.FName := ''
      else if SubProp.FName = '' then
        SubProp.FName := Format('SubProp%d', [I]);
    end;
  FKind := Value;
  Changed;
end;

procedure TXMPProperty.SetName(const Value: UnicodeString);
begin
  if Value = FName then Exit;
  Assert(Value <> '');
  FName := Value;
  Changed;
end;

procedure TXMPProperty.SetParentNamespace(Value: Boolean);
begin
  if Value = FParentNamespace then Exit;
  FParentNamespace := Value;
  if Value then
    if ParentProperty <> nil then
      FNamespaceInfo.DoAssign(ParentProperty.NamespaceInfo)
    else
      FNamespaceInfo.DoAssign(Schema.NamespaceInfo);
  Changed;
end;

procedure TXMPProperty.SetSubPropertyCount(NewCount: Integer);
var
  I: Integer;
  NewSubProp: TXMPProperty;
begin
  if NewCount < 0 then NewCount := 0;
  if NewCount = FSubProperties.Count then Exit;
  if not SupportsSubProperties then
    raise EInvalidXMPOperation.Create(SSubPropertiesNotSupported);
  if NewCount < FSubProperties.Count then
    FSubProperties.Count := NewCount
  else
    for I := FSubProperties.Count to NewCount - 1 do
    begin
      NewSubProp := TXMPProperty.Create(Schema, Self);
      if HasNamedSubProperties then NewSubProp.FName := Format('SubProp%d', [I]);
      FSubProperties.Add(NewSubProp);
    end;
  Changed;
end;

function TXMPProperty.HasNamedSubProperties: Boolean;
begin
  Result := HasNamedSubProperties(Kind);
end;

function TXMPProperty.SupportsSubProperties: Boolean;
begin
  Result := SupportsSubProperties(Kind);
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName: UnicodeString;
  SubPropKind: TXMPPropertyKind; const NewValue: UnicodeString);
var
  SubProp: TXMPProperty;
begin
  if (FSchema.Owner.UpdatePolicy = xwRemove) or (NewValue = '') then
  begin
    RemoveSubProperty(SubPropName);
    Exit;
  end;
  if not FindSubProperty(SubPropName, SubProp) then
  begin
    if FSchema.Owner.UpdatePolicy = xwUpdateIfExists then Exit;
    if not (Kind in [xpStructure, xpAltArray]) then Kind := xpStructure;
    SubProp := AddSubProperty(SubPropName)
  end;
  SubProp.Kind := SubPropKind;
  SubProp.WriteValue(NewValue);
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName, NewValue: UnicodeString);
begin
  UpdateSubProperty(SubPropName, xpSimple, NewValue);
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Integer);
begin
  UpdateSubProperty(SubPropName, xpSimple, IntToStr(NewValue));
end;

procedure TXMPProperty.UpdateSubProperty(const SubPropName: UnicodeString; NewValue: Boolean);
begin
  UpdateSubProperty(SubPropName, xpSimple, XMPBoolStrs[NewValue]);
end;

procedure TXMPProperty.WriteValue(const NewValue: UnicodeString);
var
  I, BeginPos, TotalLen: Integer;
  Strings: TUnicodeStringList;
begin
  case Kind of
    xpSimple: FValue := NewValue;
    xpStructure: raise EInvalidXMPOperation.Create(SCannotWriteSingleValueToStructureProperty);
    xpAltArray: SubProperties[DefaultLangIdent].WriteValue(NewValue);
  else
    Strings := TUnicodeStringList.Create;
    try
      BeginPos := 1;
      TotalLen := Length(NewValue);
      for I := 1 to TotalLen do
        if CharInSet(NewValue[I], [',', ';']) then
        begin
          Strings.Add(Copy(NewValue, BeginPos, I - BeginPos));
          BeginPos := I + 1;
        end;
      if BeginPos <= TotalLen then Strings.Add(Copy(NewValue, BeginPos, TotalLen));
      SubPropertyCount := Strings.Count;
      for I := 0 to Strings.Count - 1 do
        SubProperties[I].WriteValue(Strings[I]);
    finally
      Strings.Free;
    end;
  end;
end;

procedure TXMPProperty.WriteValue(const NewValue: Integer);
begin
  WriteValue(IntToStr(NewValue));
end;

procedure TXMPProperty.WriteValue(const NewValue: Boolean);
begin
  WriteValue(XMPBoolStrs[NewValue]);
end;

{ TXMPSchema }

constructor TXMPSchema.Create(AOwner: TXMPPacket; const AURI: UnicodeString);
var
  Kind: TXMPNamespace;
begin
  if TKnownXMPNamespaces.Find(AURI, Kind) then
    FNamespaceInfo := TXMPNamespaceInfo.Create(Kind)
  else
    FNamespaceInfo := TXMPNamespaceInfo.Create('', AURI);
  FNamespaceInfo.OnChange := NamespaceInfoChanged;
  FOwner := AOwner;
  FProperties := TObjectList.Create;
end;

destructor TXMPSchema.Destroy;
begin
  FNamespaceInfo.Free;
  FProperties.Free;
  inherited;
end;

function TXMPSchema.AddProperty(const AName: UnicodeString): TXMPProperty;
begin
  if NamespaceInfo.Prefix = '' then
    raise EInvalidXMPOperation.CreateRes(@SPreferredPrefixMustBeSet);
  Result := TXMPProperty.Create(Self, nil);
  FProperties.Add(Result);
  if AName <> '' then
    Result.Name := AName
  else
    Changed;
end;

procedure TXMPSchema.Changed;
begin
  if FOwner <> nil then FOwner.Changed;
end;

function TXMPSchema.FindProperty(const AName: UnicodeString; var AProperty: TXMPProperty): Boolean;
var
  I: Integer;
begin
  for I := 0 to PropertyCount - 1 do
  begin
    AProperty := Properties[I];
    if AProperty.Name = AName then
    begin
      Result := True;
      Exit;
    end;
  end;
  AProperty := nil;
  Result := False;
end;

function TXMPSchema.FindOrAddProperty(const AName: UnicodeString): TXMPProperty;
begin
  if not FindProperty(AName, Result) then
    Result := AddProperty(AName);
end;

function TXMPSchema.GetEnumerator: IXMPPropertyEnumerator;
begin
  Result := TXMPPropertyEnumerator.Create(FProperties);
end;

function TXMPSchema.GetNamespaceInfo: TXMPNamespaceInfo;
begin
  Result := FNamespaceInfo;
end;

function TXMPSchema.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TXMPSchema.GetProperty(Index: Integer): TXMPProperty;
begin
  Result := FProperties[Index];
end;

function TXMPSchema.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TXMPSchema.LoadProperty(const ASourceNode: IDOMNode): TXMPProperty;
begin
  FLoadingProperty := True;
  try
    if FProperties.Count = 0 then
      NamespaceInfo.Prefix := ASourceNode.prefix;
    Result := TXMPProperty.Create(Self, nil, ASourceNode);
    FProperties.Add(Result);
  finally
    FLoadingProperty := False;
  end;
end;

procedure TXMPSchema.NamespaceInfoChanged(Sender: TObject);
begin
  UpdateChildNamespaceInfos(Self);
  if not FLoadingProperty then Changed;
end;

function TXMPSchema.RemoveProperty(const AName: UnicodeString): Boolean;
begin
  Result := RemoveProperties([AName]);
end;

function TXMPSchema.RemoveProperties(const ANames: array of UnicodeString): Boolean;
var
  I, J: Integer;
  Prop: TXMPProperty;
begin
  Result := False;
  for I := FProperties.Count - 1 downto 0 do
  begin
    Prop := FProperties.List[I];
    for J := High(ANames) downto Low(ANames) do
      if Prop.Name = ANames[J] then
      begin
        FProperties.Delete(I);
        Result := True;
        Break;
      end;
  end;
end;

function TXMPSchema.Kind: TXMPNamespace;
begin
  Result := FNamespaceInfo.Kind;
end;

function TXMPSchema.PreferredPrefix: UnicodeString;
begin
  Result := FNamespaceInfo.Prefix;
end;

function TXMPSchema.URI: UnicodeString;
begin
  Result := FNamespaceInfo.URI;
end;

{ TXMPPacket }

constructor TXMPPacket.Create;
begin
  FSchemas := TStringListThatOwnsItsObjects.Create;
  FSchemas.CaseSensitive := False;
  FSchemas.Sorted := True;
end;

destructor TXMPPacket.Destroy;
begin
  FSchemas.Free;
  inherited;
end;

procedure TXMPPacket.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else if Source is TXMPPacket then
  begin
    RawXML := TXMPPacket(Source).RawXML;
    WriteSegmentHeader := TXMPPacket(Source).WriteSegmentHeader;
  end
  else
    inherited;
end;

procedure TXMPPacket.Changed(ResetRawXMLCache: Boolean = True);
begin
  if ResetRawXMLCache then FRawXMLCache := '';
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMPPacket.Clear(StillUpdating: Boolean);
begin
  if (FSchemas.Count = 0) and (FAboutAttributeValue = '') then Exit;
  FAboutAttributeValue := '';
  FSchemas.Clear;
  if not StillUpdating then Changed;
end;

procedure TXMPPacket.Clear;
begin
  Clear(False);
end;

function TXMPPacket.FindSchema(const URI: UnicodeString; var Schema: TXMPSchema): Boolean;
var
  Index: Integer;
begin
  Result := FSchemas.Find(URI, Index);
  if Result then Schema := FSchemas.Objects[Index] as TXMPSchema;
end;

function TXMPPacket.FindSchema(Kind: TXMPKnownNamespace; var Schema: TXMPSchema): Boolean;
begin
  Result := FindSchema(TKnownXMPNamespaces.URI[Kind], Schema);
end;

function TXMPPacket.FindOrAddSchema(const URI: UnicodeString): TXMPSchema;
var
  Index: Integer;
begin
  if FSchemas.Find(URI, Index) then
    Result := FSchemas.Objects[Index] as TXMPSchema
  else
  begin
    Result := TXMPSchema.Create(Self, URI);
    FSchemas.AddObject(URI, Result);
  end;
end;

function TXMPPacket.FindOrAddSchema(Kind: TXMPKnownNamespace): TXMPSchema;
begin
  Result := FindOrAddSchema(TKnownXMPNamespaces.URI[Kind]);
end;

procedure TXMPPacket.SetAboutAttributeValue(const Value: UnicodeString);
begin
  if Value = FAboutAttributeValue then Exit;
  FAboutAttributeValue := Value;
  Changed;
end;

function TXMPPacket.GetEmpty: Boolean;
begin
  Result := (SchemaCount = 0);
end;

function TXMPPacket.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TXMPPacket.GetRawXML: UTF8String;
var
  Stream: TMemoryStream;
begin
  if FRawXMLCache = '' then
  begin
    Stream := TMemoryStream.Create;
    try
      SaveToStream(Stream);
      SetString(FRawXMLCache, PAnsiChar(Stream.Memory), Stream.Size);
    finally
      Stream.Free;
    end;
  end;
  Result := FRawXMLCache;
end;

procedure TXMPPacket.SetRawXML(const XML: UTF8String);
var
  Stream: TUserMemoryStream;
begin
  if XML = '' then
    Clear
  else
  begin
    Stream := TUserMemoryStream.Create(Pointer(XML), Length(XML));
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

function TXMPPacket.GetSchema(Index: Integer): TXMPSchema;
begin
  Result := FSchemas.Objects[Index] as TXMPSchema;
end;

function TXMPPacket.GetSchemaCount: Integer;
begin
  Result := FSchemas.Count;
end;

procedure TXMPPacket.LoadError(Source: TStream);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, Source)
  else
    raise EInvalidXMPPacket.Create(SInvalidXMPPacket);
end;

procedure TXMPPacket.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TXMPPacket.LoadFromJPEG(const JPEGFileName: string): Boolean;
var
  Segment: IFoundJPEGSegment;
begin
  for Segment in JPEGHeader(JPEGFileName, [jmApp1]) do
    if Segment.Data.TryReadHeader(XMPSegmentHeader, SizeOf(XMPSegmentHeader)) then
    begin
      LoadFromStream(Segment.Data);
      Result := True;
      Exit;
    end;
  Clear;
  Result := False;
end;

procedure TXMPPacket.LoadFromStream(Stream: TStream);
begin
  if not TryLoadFromStream(Stream) then LoadError(Stream);
end;

function TXMPPacket.TryLoadFromStream(Stream: TStream): Boolean;
var
  I: Integer;
  CharsPtr: PAnsiChar;
  Document: IDOMDocument;
  NewStream: TMemoryStream;
  PropNode, RootRDFNode, SchemaNode: IDOMNode;
  URI: UnicodeString;
begin
  Result := False;
  WriteSegmentHeader := Stream.TryReadHeader(XMPSegmentHeader, SizeOf(XMPSegmentHeader));
  Document := GetDOM.createDocument('', '', nil);
  NewStream := TMemoryStream.Create;
  try
    NewStream.SetSize(Stream.Size - Stream.Position);
    Stream.ReadBuffer(NewStream.Memory^, NewStream.Size);
    CharsPtr := NewStream.Memory;
    for I := NewStream.Size - 1 downto 0 do //MSXML chokes on embedded nulls
      if CharsPtr[I] = #0 then CharsPtr[I] := ' ';
    if not (Document as IDOMPersist).loadFromStream(NewStream) then Exit;
    if not FindRootRDFNode(Document, RootRDFNode) then Exit;
    Clear(True);
    if StrLComp(CharsPtr, PAnsiChar('<?xpacket '), 10) = 0 then
      SetString(FRawXMLCache, CharsPtr, NewStream.Size)
    else
      FRawXMLCache := UTF8Encode((Document as IDOMPersist).xml)
  finally
    NewStream.Free;
  end;
  SchemaNode := RootRDFNode.firstChild;
  while SchemaNode <> nil do
  begin
    if (SchemaNode.nodeType = ELEMENT_NODE) and (SchemaNode.namespaceURI = RDF.URI) and
      UnicodeSameText(SchemaNode.nodeName, RDF.DescriptionNodeName) then
    begin
      if FAboutAttributeValue = '' then
        with SchemaNode as IDOMElement do
        begin
          FAboutAttributeValue := getAttributeNS(RDF.URI, RDF.AboutAttrLocalName);
          if FAboutAttributeValue = '' then
            FAboutAttributeValue := getAttribute(RDF.AboutAttrLocalName);
        end;
      //look for tags stored as attributes
      for I := 0 to SchemaNode.attributes.length - 1 do
      begin
        PropNode := SchemaNode.attributes.item[I];
        if DefinesNS(PropNode) then Continue;
        URI := PropNode.namespaceURI;
        if (URI <> '') and (URI <> RDF.URI) then
          Schemas[URI].LoadProperty(PropNode);
      end;
      //look for tags stored as element nodes
      PropNode := SchemaNode.firstChild;
      while PropNode <> nil do
      begin
        URI := PropNode.namespaceURI;
        if (URI <> '') and (PropNode.nodeType = ELEMENT_NODE) then
          Schemas[URI].LoadProperty(PropNode);
        PropNode := PropNode.nextSibling;
      end;
    end;
    SchemaNode := SchemaNode.nextSibling;
  end;
  Changed(False);
  Result := True;
end;

procedure TXMPPacket.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXMPPacket.SaveToStream(Stream: TStream);

  procedure WriteProps(Schema: TXMPSchema; Level: Integer;
    const Props: IXMPPropertyCollection);
  const
    PropNodeStart: UnicodeString = #9#9#9'%s<%s:%s>'#10;
    PropNodeEnd: UnicodeString   = #9#9#9'%s</%s:%s>'#10;
    SimpleFmt: UnicodeString = #9#9#9'%s<%s>%s</%1s>'#10;
  var
    Indent, QualName, NSDecl, RDFName, Value: UnicodeString;
    Prop: TXMPProperty;
    IsArrayElem: Boolean;
  begin
    Indent := StringOfChar(WideChar(#9), Level);
    for Prop in Props do
    begin
      IsArrayElem := (Prop.ParentProperty <> nil) and (Prop.ParentProperty.Kind <> xpStructure);
      if IsArrayElem or Prop.ParentNamespace then
        NSDecl := '' //inherit from parent
      else
        NSDecl := UnicodeFormat(' xmlns:%s="%s"', [Prop.NamespaceInfo.Prefix,
          Prop.NamespaceInfo.URI]);
      QualName := Prop.NamespaceInfo.Prefix + ':' + Prop.Name;
      if IsArrayElem then
      begin
        if Prop.ParentProperty.Kind = xpAltArray then
          Value := UnicodeFormat(' xml:lang="%s"', [Prop.Name])
        else
          Value := '';
        Stream.WriteUTF8Chars('%s<%s%s>', [Indent, RDF.ListNodeName, Value]);
      end;
      if Prop.Kind = xpSimple then
      begin
        Value := EscapeXML(Prop.ReadValue);
        if IsArrayElem then
          Stream.WriteUTF8Chars('%s</%s>'#10, [Value, RDF.ListNodeName])
        else
          Stream.WriteUTF8Chars('%s<%s%s>%s</%1:s>'#10, [Indent, QualName, NSDecl, Value]);
        Continue;
      end;
      if IsArrayElem then
        Stream.WriteByte(#10)
      else
        Stream.WriteUTF8Chars('%s<%s%s>'#10, [Indent, QualName, NSDecl]);
      case Prop.Kind of
        xpStructure: RDFName := RDF.DescriptionNodeName;
        xpAltArray: RDFName := RDF.AltNodeName;
        xpBagArray: RDFName := RDF.BagNodeName;
        xpSeqArray: RDFName := RDF.SeqNodeName;
      else Assert(False);
      end;
      Stream.WriteUTF8Chars('%s'#9'<%s>'#10, [Indent, RDFName]);
      WriteProps(Schema, Level + 2, Prop);
      Stream.WriteUTF8Chars('%s'#9'</%s>'#10, [Indent, RDFName]);
      if IsArrayElem then
        Stream.WriteUTF8Chars('%s</%s>'#10, [Indent, RDF.ListNodeName])
      else
        Stream.WriteUTF8Chars('%s</%s>'#10, [Indent, QualName]);
    end;
  end;
const
  PacketStart: UTF8String =
    '<?xpacket begin="" id="W5M0MpCehiHzreSzNTczkc9d"?>'#10 +
    '<x:xmpmeta xmlns:x="adobe:ns:meta/" CCRExifVersion="' + CCRExifVersion + '">'#10 +
    #9'<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">'#10;
  PacketEnd: UTF8String =
    #9'</rdf:RDF>'#10 +
    '</x:xmpmeta>'#10 +
    '<?xpacket end="w"?> ';      //The packet wrapper (i.e., the <?xpacket lines) are optional according to the XMP spec,
  PaddingByte: AnsiChar = ' ';   //but required by Windows Explorer in Vista. Vista also needs a trailing space character,
  DescNodeStart: UnicodeString = //else it doesn't read and raises a spurious error when the user tries to write.
    #9#9'<rdf:Description rdf:about="%s" xmlns:%s="%s">'#10;
  DescNodeEnd: UTF8String =
    #9#9'</rdf:Description>'#10;
var
  Schema: TXMPSchema;
begin
  if WriteSegmentHeader then
    Stream.WriteBuffer(XMPSegmentHeader, SizeOf(XMPSegmentHeader));
  if FRawXMLCache <> '' then
  begin
    Stream.WriteUTF8Chars(FRawXMLCache);
    if FRawXMLCache[Length(FRawXMLCache)] <> ' ' then
      Stream.WriteBuffer(PaddingByte, 1); //see note above
    Exit;
  end;
  Stream.WriteUTF8Chars(PacketStart);
  for Schema in Self do
  begin
    Stream.WriteUTF8Chars(DescNodeStart, [AboutAttributeValue,
      Schema.NamespaceInfo.Prefix, Schema.NamespaceInfo.URI]);
    WriteProps(Schema, 3, Schema);
    Stream.WriteUTF8Chars(DescNodeEnd);
  end;
  Stream.WriteUTF8Chars(PacketEnd);
end;

procedure TXMPPacket.RemoveProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString);
var
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) then Schema.RemoveProperty(PropName);
end;

procedure TXMPPacket.RemoveProperties(SchemaKind: TXMPKnownNamespace;
  const PropNames: array of UnicodeString);
var
  Schema: TXMPSchema;
begin
  if FindSchema(SchemaKind, Schema) then Schema.RemoveProperties(PropNames);
end;

procedure TXMPPacket.DoUpdateProperty(Policy: TXMPWritePolicy; SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; PropKind: TXMPPropertyKind; const NewValue: UnicodeString);
var
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  if (Policy = xwRemove) or (NewValue = '') then
  begin
    RemoveProperty(SchemaKind, PropName);
    Exit;
  end;
  if Policy = xwAlwaysUpdate then
    Schema := FindOrAddSchema(SchemaKind)
  else
    if not FindSchema(SchemaKind, Schema) then Exit;
  if not Schema.FindProperty(PropName, Prop) then
    if Policy = xwAlwaysUpdate then
      Prop := Schema.AddProperty(PropName)
    else
      Exit;
  Prop.Kind := PropKind;
  Prop.WriteValue(TrimRight(NewValue));
end;

procedure TXMPPacket.DoUpdateArrayProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; ArrayPropKind: TXMPPropertyKind;
  const NewValues: array of UnicodeString);
var
  I, NewSubPropCount: Integer;
  Prop: TXMPProperty;
  Schema: TXMPSchema;
begin
  NewSubPropCount := Length(NewValues);
  if (UpdatePolicy = xwRemove) or (NewSubPropCount = 0) then
  begin
    RemoveProperty(SchemaKind, PropName);
    Exit;
  end;                                   
  if UpdatePolicy = xwAlwaysUpdate then
    Schema := FindOrAddSchema(SchemaKind)
  else
    if not FindSchema(SchemaKind, Schema) then Exit;
  if not Schema.FindProperty(PropName, Prop) then
    if UpdatePolicy = xwAlwaysUpdate then
      Prop := Schema.AddProperty(PropName)
    else
      Exit;
  if not (Prop.Kind in [xpSimple, xpBagArray, xpSeqArray]) then
  begin
    RemoveProperty(SchemaKind, PropName);
    Prop := Schema.AddProperty(PropName);
  end;
  Prop.Kind := ArrayPropKind;
  Prop.SubPropertyCount := NewSubPropCount;
  for I := 0 to NewSubPropCount - 1 do
    Prop[I].WriteValue(NewValues[I]);
end;

procedure TXMPPacket.UpdateProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; PropKind: TXMPPropertyKind; const NewValue: UnicodeString);
var
  SubPolicy: TXMPWritePolicy;
begin
  DoUpdateProperty(UpdatePolicy, SchemaKind, PropName, PropKind, NewValue);
  if (SchemaKind <> xsTIFF) or (PropKind <> xpSimple) or
     (PropName[1] <> UpCase(PropName[1])) then Exit;
  { Special handling for the TIFF schema - basically, the spec has its properties all
    with an initial capital, but Vista (perhaps for bkwards compat reasons) can write
    to both this *and* an all lowercase version. }
  SubPolicy := UpdatePolicy;
  if SubPolicy = xwAlwaysUpdate then SubPolicy := xwUpdateIfExists;
  DoUpdateProperty(SubPolicy, SchemaKind, LowerCase(PropName), PropKind, NewValue);
end;

procedure TXMPPacket.UpdateBagProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValues: array of UnicodeString);
begin
  DoUpdateArrayProperty(SchemaKind, PropName, xpBagArray, NewValues);
end;

procedure TXMPPacket.UpdateBagProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValueList: UnicodeString);
begin
  UpdateProperty(SchemaKind, PropName, xpBagArray, NewValueList);
end;

procedure TXMPPacket.UpdateSeqProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValues: array of UnicodeString);
begin
  DoUpdateArrayProperty(SchemaKind, PropName, xpSeqArray, NewValues);
end;

procedure TXMPPacket.UpdateSeqProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValueList: UnicodeString);
begin
  UpdateProperty(SchemaKind, PropName, xpSeqArray, NewValueList);
end;

procedure TXMPPacket.UpdateProperty(SchemaKind: TXMPKnownNamespace;
  const PropName, NewValue: UnicodeString);
begin
  UpdateProperty(SchemaKind, PropName, xpSimple, NewValue);
end;

procedure TXMPPacket.UpdateProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValue: Integer);
begin
  UpdateProperty(SchemaKind, PropName, xpSimple, IntToStr(NewValue));
end;

procedure TXMPPacket.UpdateDateTimeProperty(SchemaKind: TXMPKnownNamespace;
  const PropName: UnicodeString; const NewValue: TDateTime; ApplyLocalBias: Boolean);
var
  S: UnicodeString;
begin
  if NewValue = 0 then
    RemoveProperty(SchemaKind, PropName)
  else
  begin
    S := DateTimeToXMLTime(NewValue, ApplyLocalBias);
    UpdateProperty(SchemaKind, PropName, xpSimple, S);
  end;
end;

{ TXMPPacket.TEnumerator }

constructor TXMPPacket.TEnumerator.Create(Packet: TXMPPacket);
begin
  FPacket := Packet;
  FIndex := -1;
end;

function TXMPPacket.TEnumerator.GetCurrent: TXMPSchema;
begin
  Result := FPacket[FIndex];
end;

function TXMPPacket.TEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FIndex < FPacket.SchemaCount);
end;

initialization
{$IFDEF MSWINDOWS}
  if IsConsole and Assigned(InitProc) then
  begin                   //TXMPPacket implicitly uses MSXML, which requires CoInitialize
    TProcedure(InitProc); //or CoInitializeEx to be called. This will be done
    InitProc := nil;      //automatically with a VCL app (the RTL's MSXML wrapper uses
  end;                    //ComObj.pas, which assigns InitProc, which is called by
{$ENDIF}                  //Application.Initialize), but not in a console one.
finalization
  TKnownXMPNamespaces.FKnownURIs.Free;
end.
