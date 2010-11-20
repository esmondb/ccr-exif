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
{ The Original Code is XMPBrowserFrame.pas.                                            }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit XMPBrowserFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Tabs, CCR.Exif, CCR.Exif.JPEGUtils, CCR.Exif.XMPUtils;

type
  TTabSet = class(Tabs.TTabSet) //work around a bug that is actually TPageControl's
  protected
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  end;

  TOutputFrame = class(TFrame)
    TabSet: TTabSet;
    panProps: TPanel;
    TreeView: TTreeView;
    Splitter: TSplitter;
    memValue: TMemo;
    redRawXML: TRichEdit;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewClick(Sender: TObject);
    procedure TabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
  private
    FXMPPacket: TXMPPacket;
    procedure LoadPacket(Source: TCustomMemoryStream);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
  end;

implementation

uses StrUtils, Themes, CCR.Exif.Consts;

{$R *.dfm}

function GetNamespaceTitle(Schema: TXMPSchema): string;
begin
  case Schema.NamespaceInfo.Kind of
    xsCameraRaw: Result := 'Camera Raw';
    xsColorant: Result := 'Dublin Core';
    xsDimensions: Result := 'Dimensions';
    xsDublinCore: Result := 'Dublin Core';
    xsExif: Result := 'Exif';
    xsExifAux: Result := 'Exif (Additional)';
    xsIPTC: Result := 'IPTC';
    xsMicrosoftPhoto: Result := 'Microsoft Photo';
    xsPDF: Result := 'PDF';
    xsPhotoshop: Result := 'Photoshop';
    xsResourceEvent: Result := 'Resource Event';
    xsResourceRef: Result := 'Resource Reference';
    xsTIFF: Result := 'TIFF';
    xsXMPBasic: Result := 'XMP Basic';
    xsXMPBasicJobTicket: Result := 'XMP Basic Job Ticket';
    xsXMPDynamicMedia: Result := 'XMP Dynamic Media';
    xsXMPMediaManagement: Result := 'XMP Media Management';
    xsXMPPagedText: Result := 'XMP Paged Text';
    xsXMPRights: Result := 'XMP Rights';
  else
    Result := Schema.NamespaceInfo.Prefix;
  end;
end;

{ TTabSet }

procedure TTabSet.CMDialogChar(var Message: TCMDialogChar);
var
  Grandparent: TWinControl;
begin
  Grandparent := Parent.Parent;
  if not (Grandparent is TTabSheet) or (TTabSheet(Grandparent).PageControl.ActivePage = Grandparent) then
    inherited;
end;

{ TOutputFrame }

constructor TOutputFrame.Create(AOwner: TComponent);
begin
  inherited;
  TabSet.Tabs.Objects[0] := panProps;
  TabSet.Tabs.Objects[1] := redRawXML;
  FXMPPacket := TXMPPacket.Create;
end;

destructor TOutputFrame.Destroy;
begin
  FXMPPacket.Free;
  inherited;
end;

procedure TOutputFrame.LoadPacket(Source: TCustomMemoryStream);
  procedure LoadNodes(Parent: TTreeNode; const Properties: IXMPPropertyCollection);
  var
    Counter: Integer;
    Prop: TXMPProperty;
    S: string;
  begin
    Counter := 0;
    for Prop in Properties do //both TXMPSchema and TXMPProperty implement IXMPPropertyCollection
    begin
      Inc(Counter);
      if (Prop.ParentProperty = nil) or not (Prop.ParentProperty.Kind in [xpBagArray, xpSeqArray]) then
        S := Prop.Name
      else
        S := Format('<item %d>', [Counter]);
      LoadNodes(TreeView.Items.AddChildObject(Parent, S, Prop), Prop);
    end;
  end;
const
  TabStops: array[0..0] of UINT = (8);
var
  Schema: TXMPSchema;
begin
  SendMessage(redRawXML.Handle, EM_SETTABSTOPS, Length(TabStops), LPARAM(@TabStops));
  if not FXMPPacket.TryLoadFromStream(Source) then
  begin //still display the XML (helpful for debugging purposes)
    if CompareMem(Source.Memory, @XMPSegmentHeader, SizeOf(XMPSegmentHeader)) then
      Source.Position := SizeOf(XMPSegmentHeader)
    else
      Source.Position := 0;
    redRawXML.Text := UTF8ToString(@PAnsiChar(Source.Memory)[Source.Position],
      Source.Size - Source.Position);
    TabSet.TabIndex := 1;
    MessageDlg(SInvalidXMPPacket, mtError, [mbOK], 0);
    Exit;
  end;
  redRawXML.Text := UTF8ToString(FXMPPacket.RawXML);
  TreeView.Items.Add(nil, '''About'' attribute');
  for Schema in FXMPPacket do
    LoadNodes(TreeView.Items.AddObject(nil, GetNamespaceTitle(Schema), Schema), Schema);
  TreeView.AlphaSort;
  if TreeView.Items.Count > 0 then
    TreeView.Items[0].Expand(False);
end;

procedure TOutputFrame.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent <> nil then
  begin
    TabSet.ParentFont := True;
    TabSet.Height := TabSet.Canvas.TextHeight('X') + 4;
    if (AParent is TTabSheet) and ThemeServices.ThemesEnabled then
      TabSet.BackgroundColor := clWindow
    else
      TabSet.BackgroundColor := clBtnFace;
  end;
end;

procedure TOutputFrame.LoadFromFile(const FileName: string);
var
  Stream: TMemoryStream;
  Segment: IFoundJPEGSegment;
begin
  Stream := nil;
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    memValue.Clear;
    FXMPPacket.Clear;
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);
    if MatchText(ExtractFileExt(FileName), ['.xmp', '.xml', '.txt']) then
      LoadPacket(Stream)
    else if HasJPEGHeader(Stream) then
    begin
      for Segment in JPEGHeader(Stream, [jmApp1]) do
        if HasXMPSegmentHeader(Segment.Data) then
        begin
          LoadPacket(Segment.Data);
          Exit;
        end;
      MessageDlg('Image does not contain any XMP metadata', mtInformation, [mbOK], 0);
    end
    else
      raise EInvalidXMPPacket.CreateFmt('%s is neither a JPEG image nor an XMP ' +
        'sidecar file', [FileName]);
  finally
    Stream.Free;
    TreeView.Items.EndUpdate;
  end;
end;

procedure TOutputFrame.TabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  NewPanel: TWinControl;
begin
  NewPanel := (TabSet.Tabs.Objects[NewTab] as TWinControl);
  NewPanel.Show;
  NewPanel.BringToFront;
  NewPanel.SetFocus;
  (TabSet.Tabs.Objects[TabSet.TabIndex] as TControl).Hide;
end;

procedure TOutputFrame.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if TObject(Node.Data) is TXMPProperty then
    case TXMPProperty(Node.Data).Kind of
      xpAltArray: memValue.Text := '<alternative array property>';
      xpBagArray: memValue.Text := '<unordered array property>';
      xpSeqArray: memValue.Text := '<ordered array property>';
      xpStructure: memValue.Text := '<structure property>';
    else
      memValue.Text := TXMPProperty(Node.Data).ReadValue
    end
  else if Node.IsFirstNode then
    memValue.Text := FXMPPacket.AboutAttributeValue
  else
    memValue.Text := ''
end;

procedure TOutputFrame.TreeViewClick(Sender: TObject);
var
  Node: TTreeNode;
  Pos: TPoint;
begin
  Pos := TreeView.ScreenToClient(Mouse.CursorPos);
  Node := TreeView.GetNodeAt(Pos.X, Pos.Y);
  if (Node <> nil) and (Node = TreeView.Selected) and (TreeView.Items.Count > 1) and
     PtInRect(Node.DisplayRect(True), Pos) then
    Node.Expanded := not Node.Expanded;
end;

end.
