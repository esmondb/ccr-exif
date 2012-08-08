{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.1                                                                        }
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
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit XMPBrowserFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ComCtrls, StdCtrls, ExtCtrls, Tabs, CCR.Exif.BaseUtils, CCR.Exif.XMPUtils;

type
  TTabSet = class(Tabs.TTabSet) //work around a bug that is actually TPageControl's
  protected
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  end;

  IOutputFrameOwner = interface
  ['{01E38A15-3573-49EA-82A0-9AC6712A14E1}']
    procedure ActiveURIChanged(const NewURI: string);
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
    mnuEdit: TPopupMenu;
    itmCopySelText: TMenuItem;
    itmSelectAll: TMenuItem;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewClick(Sender: TObject);
    procedure TabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure itmSelectAllClick(Sender: TObject);
    procedure itmCopySelTextClick(Sender: TObject);
    procedure mnuEditPopup(Sender: TObject);
  private
    FLastURI: string;
    FXMPPacket: TXMPPacket;
    procedure XMPPacketLoadError(Sender: TXMPPacket; Source: TStream);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const FileName: string);
  end;

implementation

uses ClipBrd, StrUtils, CCR.Exif.Consts, CCR.Exif.Demos;

{$R *.dfm}

resourcestring
  SFileNotSupported = '%s is neither of a supported image type nor an XMP sidecar file';
  SImageHasNoXMP = 'Image does not contain any XMP metadata';

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
  FXMPPacket := TXMPPacket.Create(Self);
  FXMPPacket.OnLoadError := XMPPacketLoadError; //if OnLoadError isn't handled, an exception will be raised in the normal manner when a packet doesn't load properly
end;

procedure TOutputFrame.itmCopySelTextClick(Sender: TObject);
begin
  Clipboard.AsText := (Screen.ActiveControl as TCustomEdit).SelText;
end;

procedure TOutputFrame.itmSelectAllClick(Sender: TObject);
begin
  (Screen.ActiveControl as TCustomEdit).SelectAll;
end;

procedure TOutputFrame.mnuEditPopup(Sender: TObject);
begin
  itmSelectAll.Enabled := (Screen.ActiveControl is TCustomEdit) and
    (TCustomEdit(Screen.ActiveControl).GetTextLen > 0);
  itmCopySelText.Enabled := itmSelectAll.Enabled and
    (TCustomEdit(Screen.ActiveControl).SelLength > 0);
end;

procedure TOutputFrame.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent <> nil then
  begin
    TabSet.ParentFont := True;
    TabSet.Height := TabSet.Canvas.TextHeight('X') + 4;
    if (AParent is TTabSheet) and IsThemingEnabled then
      TabSet.BackgroundColor := clWindow
    else
      TabSet.BackgroundColor := clBtnFace;
  end;
end;

procedure TOutputFrame.LoadFromFile(const FileName: string);
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
  Stream: TMemoryStream;
begin
  (Owner as IOutputFrameOwner).ActiveURIChanged('');
  Stream := nil;
  TreeView.Items.BeginUpdate;
  try
    Screen.Cursor := crHourGlass;
    TreeView.Items.Clear;
    memValue.Clear;
    FXMPPacket.Clear;
    redRawXML.Clear;
    SendMessage(redRawXML.Handle, EM_SETTABSTOPS, Length(TabStops), LPARAM(@TabStops));
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);
    if FXMPPacket.LoadFromGraphic(Stream) then //if LoadFromGraphic returns True, then the graphic was of a supported format
    begin
      if FXMPPacket.Empty then
      begin
        MessageDlg(SImageHasNoXMP, mtInformation, [mbOK], 0);
        Exit;
      end;
    end
    else if not FXMPPacket.TryLoadFromStream(Stream) then
    begin
      MessageDlg(Format(SFileNotSupported, [FileName]), mtError, [mbOK], 0);
      Exit;
    end;
    redRawXML.Text := UTF8ToString(FXMPPacket.RawXML);
    TreeView.Items.Add(nil, '''About'' attribute (frequently blank)');
    for Schema in FXMPPacket do
      LoadNodes(TreeView.Items.AddObject(nil, GetNamespaceTitle(Schema), Schema), Schema);
    TreeView.AlphaSort;
    if TreeView.Items.Count > 0 then
      TreeView.Items[0].Expand(False);
  finally
    Stream.Free;
    TreeView.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TOutputFrame.XMPPacketLoadError(Sender: TXMPPacket; Source: TStream);
var
  MemStream: TCustomMemoryStream;
begin //still display the XML when couldn't actually load it (helpful for debugging purposes)
  MemStream := Source as TCustomMemoryStream;
  if CompareMem(MemStream.Memory, @TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader)) then
    MemStream.Position := SizeOf(TJPEGSegment.XMPHeader)
  else
    MemStream.Position := 0;
  redRawXML.Text := UTF8ToString(@PAnsiChar(MemStream.Memory)[MemStream.Position],
    MemStream.Size - MemStream.Position);
  TabSet.TabIndex := 1;
  MessageDlg(SInvalidXMPPacket, mtError, [mbOK], 0);
  Abort;
end;

type
  TWinControlAccess = class(TWinControl);

procedure TOutputFrame.TabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  Page: TWinControl;
begin
  Page := (TabSet.Tabs.Objects[NewTab] as TWinControl);
  Page.Show;
  Page.BringToFront;
  if Page.ControlCount = 0 then
    Page.SetFocus
  else
    TWinControlAccess(Page).SelectFirst;
  (TabSet.Tabs.Objects[TabSet.TabIndex] as TControl).Hide;
  if NewTab = 0 then
    (Owner as IOutputFrameOwner).ActiveURIChanged(FLastURI)
  else
    (Owner as IOutputFrameOwner).ActiveURIChanged('http://ns.adobe.com/xap/1.0/');
end;

procedure TOutputFrame.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if TObject(Node.Data) is TXMPProperty then
  begin
    case TXMPProperty(Node.Data).Kind of
      xpAltArray: memValue.Text := '<alternative array property>';
      xpBagArray: memValue.Text := '<unordered array property>';
      xpSeqArray: memValue.Text := '<ordered array property>';
      xpStructure: memValue.Text := '<structure property>';
    else
      memValue.Text := TXMPProperty(Node.Data).ReadValue
    end;
    FLastURI := TXMPProperty(Node.Data).NamespaceInfo.URI;
  end
  else if Node.IsFirstNode then
  begin
    memValue.Text := FXMPPacket.AboutAttributeValue;
    FLastURI := 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
  end
  else
  begin
    memValue.Text := '';
    FLastURI := (TObject(Node.Data) as TXMPSchema).NamespaceInfo.URI;
  end;
  (Owner as IOutputFrameOwner).ActiveURIChanged(FLastURI);
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
