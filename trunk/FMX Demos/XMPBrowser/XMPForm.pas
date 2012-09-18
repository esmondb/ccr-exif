unit XMPForm;

{.$DEFINE FORCEADOM}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.Memo, FMX.TreeView,
  CCR.Exif.BaseUtils, CCR.Exif.Consts, CCR.Exif.XMPUtils, CCR.Exif.FMXUtils, XMPController;

type
  TfrmXMPBrowser = class(TForm, IXMPDocumentForm)
    TabControl: TTabControl;
    tabProps: TTabItem;
    tabRawXML: TTabItem;
    lyoButtonBar: TLayout;
    btnOpenFile: TButton;
    btnClose: TButton;
    tabImage: TTabItem;
    trvProps: TTreeView;
    Splitter1: TSplitter;
    memValue: TMemo;
    memRawXML: TMemo;
    imgImage: TImageControl;
    lblURI: TLabel;
    StatusBar: TStatusBar;
    procedure TabControlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trvPropsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure trvPropsDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    FDoneDragEnter, FHandleDragDirectly: Boolean;
    FFileName: string;
    FLoadedImage: Boolean;
    FRotateImageBy: Single;
    FLastURI: string;
    FXMPPacket: TXMPPacket;
    procedure ActiveURIChanged(const NewURI: string);
    procedure XMPPacketLoadError(Sender: TXMPPacket; Source: TStream);
  strict protected
    function GetFileName: string;
    procedure GotoNextTab;
    procedure GotoPreviousTab;
    procedure OpenFile(const AFileName: string);
  protected
    procedure CreateHandle; override;
  public
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Accept: Boolean); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    procedure DragLeave; override;
  end;

implementation

{$IFDEF FORCEADOM}
uses Xml.Xmldom, Xml.adomxmldom;
{$ENDIF}

{$R *.fmx}

resourcestring
  SFileNotSupported = 'File is not of a supported type (%s)';
  SImageHasNoXMP = 'Image does not contain any XMP metadata (%s)';

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

{ TfrmXMPBrowser }

procedure TfrmXMPBrowser.FormCreate(Sender: TObject);
begin
  FXMPPacket := TXMPPacket.Create(Self);
  FXMPPacket.OnLoadError := XMPPacketLoadError; //if OnLoadError isn't handled, an exception will be raised in the normal manner when a packet doesn't load properly
  lyoButtonBar.Visible := (TFileManager.DocumentMode = dmOnePerAppInst);
  StatusBar.Visible := not lyoButtonBar.Visible;
  if StatusBar.Visible then
  begin
    lblURI.Parent := StatusBar;
    lblURI.Align := TAlignLayout.alClient;
  end;
end;

procedure TfrmXMPBrowser.FormShow(Sender: TObject);
begin
  OnShow := nil;
  if lyoButtonBar.Visible then
  begin
    btnOpenFile.Action := dtmController.actOpenFile;
    btnClose.Action := dtmController.actClose;
  end;
end;

procedure TfrmXMPBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfrmXMPBrowser.CreateHandle;
begin
  inherited;
  TMacCommands.EnableFullScreen(Self);
end;

procedure TfrmXMPBrowser.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  { The default behaviour is to pass the event on to the control being hovered
    over. Handling the drag operation at the form level just means it doesn't
    matter what control files are dropped onto. }
  FDoneDragEnter := True;
  FHandleDragDirectly := (Data.Files <> nil);
  if not FHandleDragDirectly then inherited;
end;

procedure TfrmXMPBrowser.DragOver(const Data: TDragObject; const Point: TPointF;
  var Accept: Boolean);
begin
  if not FDoneDragEnter then //!!!FMX bug - DragEnter never called on Windows
    DragEnter(Data, Point);
  if FHandleDragDirectly then
    Accept := True
  else
    inherited;
end;

procedure TfrmXMPBrowser.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  if FHandleDragDirectly then
    TFileManager.OpenDroppedFiles(Data.Files)
  else
    inherited;
  FDoneDragEnter := False;
end;

procedure TfrmXMPBrowser.DragLeave;
begin
  FDoneDragEnter := False;
  inherited;
end;

function TfrmXMPBrowser.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TfrmXMPBrowser.GotoNextTab;
begin
  if TabControl.TabIndex = TabControl.TabCount - 1 then
    TabControl.TabIndex := 0
  else
    TabControl.TabIndex := TabControl.TabIndex + 1;
end;

procedure TfrmXMPBrowser.GotoPreviousTab;
begin
  if TabControl.TabIndex = 0 then
    TabControl.TabIndex := TabControl.TabCount - 1
  else
    TabControl.TabIndex := TabControl.TabIndex - 1;
end;

function NewTreeViewItem(const AParent: TFmxObject; const AText: string;
  const ATagObject: TObject = nil): TTreeViewItem;
begin
  Result := TTreeViewItem.Create(AParent);
  try
    Result.Text := AText;
    Result.TagObject := ATagObject;
    AParent.AddObject(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TfrmXMPBrowser.OpenFile(const AFileName: string);
  procedure LoadNodes(const AParent: TTreeViewItem; const AProperties: IXMPPropertyCollection);
  var
    Counter: Integer;
    Prop: TXMPProperty;
    S: string;
  begin
    Counter := 0;
    for Prop in AProperties do //both TXMPSchema and TXMPProperty implement IXMPPropertyCollection
    begin
      Inc(Counter);
      if (Prop.ParentProperty <> nil) and (Prop.ParentProperty.Kind in [xpBagArray, xpSeqArray]) then
        S := Format('<item %d>', [Counter])
      else
        S := Prop.Name;
      LoadNodes(NewTreeViewItem(AParent, S, Prop), Prop);
    end;
  end;
var
  Schema: TXMPSchema;
  Stream: TMemoryStream;
begin
  ActiveURIChanged('');
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(AFileName);
    if FXMPPacket.LoadFromGraphic(Stream) then //if LoadFromGraphic returns True, then the graphic was of a supported format
    begin
      if FXMPPacket.Empty then
      begin
        MessageDlg(Format(SImageHasNoXMP, [AFileName]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
        Abort;
      end;
    end
    else if not FXMPPacket.TryLoadFromStream(Stream) then
    begin
      MessageDlg(Format(SFileNotSupported, [AFileName]), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      Abort;
    end;
  finally
    Stream.Free;
  end;
  memValue.Text := '';
  memRawXML.Text := UTF8ToString(FXMPPacket.RawXML);
  trvProps.BeginUpdate;
  try
    trvProps.Clear;
    trvProps.Sorted := False;
    NewTreeViewItem(trvProps, '''About'' attribute (frequently blank)');
    for Schema in FXMPPacket do
      LoadNodes(NewTreeViewItem(trvProps, GetNamespaceTitle(Schema), Schema), Schema);
    trvProps.Sorted := True;
    if trvProps.Count > 0 then
      trvProps.ItemByIndex(0).IsExpanded := True;
  finally
    trvProps.EndUpdate;
  end;
  FFileName := AFileName;
  FLoadedImage := False;
  TabControlChange(nil);
end;

procedure TfrmXMPBrowser.TabControlChange(Sender: TObject);
begin
  if tabImage.IsSelected and not FLoadedImage and (FFileName <> '') then
  begin
    imgImage.Bitmap.LoadFromFile(FFileName);
    if FRotateImageBy <> 0 then imgImage.Bitmap.Rotate(FRotateImageBy);
    FLoadedImage := True;
  end;
  case TabControl.TabIndex of
    0: ActiveURIChanged(FLastURI);
    1: ActiveURIChanged('http://ns.adobe.com/xap/1.0/');
    2: ActiveURIChanged(FFileName);
  else Assert(False, 'Unexpected TabIndex value');
  end;
end;

procedure TfrmXMPBrowser.trvPropsChange(Sender: TObject);
var
  Prop: TXMPProperty;
begin
  if trvProps.Selected = nil then Exit;
  if trvProps.Selected.TagObject is TXMPProperty then
  begin
    Prop := TXMPProperty(trvProps.Selected.TagObject);
    case Prop.Kind of
      xpAltArray: memValue.Text := '<alternative array property>';
      xpBagArray: memValue.Text := '<unordered array property>';
      xpSeqArray: memValue.Text := '<ordered array property>';
      xpStructure: memValue.Text := '<structure property>';
    else
      memValue.Text := Prop.ReadValue
    end;
    FLastURI := Prop.NamespaceInfo.URI;
  end
  else if trvProps.Selected.TagObject is TXMPSchema then
  begin
    memValue.Text := '';
    FLastURI := TXMPSchema(trvProps.Selected.TagObject).NamespaceInfo.URI;
  end
  else
  begin
    memValue.Text := FXMPPacket.AboutAttributeValue;
    FLastURI := 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
  end;
  ActiveURIChanged(FLastURI);
end;

procedure TfrmXMPBrowser.trvPropsDblClick(Sender: TObject);
begin
  if trvProps.Selected <> nil then
    trvProps.Selected.IsExpanded := not trvProps.Selected.IsExpanded;
end;

procedure TfrmXMPBrowser.ActiveURIChanged(const NewURI: string);
begin
  lblURI.Text := NewURI;
end;

procedure TfrmXMPBrowser.XMPPacketLoadError(Sender: TXMPPacket; Source: TStream);
var
  MemStream: TCustomMemoryStream;
begin //still display the XML when couldn't actually load it (helpful for debugging purposes)
  MemStream := Source as TCustomMemoryStream;
  if CompareMem(MemStream.Memory, @TJPEGSegment.XMPHeader, SizeOf(TJPEGSegment.XMPHeader)) then
    MemStream.Position := SizeOf(TJPEGSegment.XMPHeader)
  else
    MemStream.Position := 0;
  memRawXML.Text := UTF8ToString(@PAnsiChar(MemStream.Memory)[MemStream.Position],
    MemStream.Size - MemStream.Position);
  TabControl.TabIndex := 1;
  MessageDlg(SInvalidXMPPacket, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  Abort;
end;

initialization
  TfrmXMPBrowser.ClassName; //ensure class is linked in!
  {$IFDEF FORCEADOM}
  DefaultDOMVendor := sAdom4XmlVendor;
  {$ENDIF}
end.
