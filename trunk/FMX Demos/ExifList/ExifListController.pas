unit ExifListController;

interface

uses
  System.Types, System.UITypes, System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  System.Actions, FMX.ActnList, FMX.StdActns, FMX.Forms, FMX.Types, FMX.Dialogs, FMX.Platform, FMX.Menus,
  CCR.Exif.FMXUtils;

type
  IExifDocumentForm = interface
  ['{AB25321B-E8DD-49E0-B796-7747B3038C91}']
    function GetFileName: string;
    function CanCopyTags: Boolean;
    procedure CopyTags(const AService: IFMXClipboardService);
    procedure GotoNextTab;
    procedure GotoPreviousTab;
    procedure OpenFile(const AFileName: string);
    property FileName: string read GetFileName;
  end;

  TdtmController = class(TDataModule)
    ActionList: TActionList;
    actCopyTags: TAction;
    actOpenInDefProg: TAction;
    actOpenFile: TAction;
    actClose: TWindowClose;
    dlgOpen: TOpenDialog;
    MacMenu: TMainMenu;
    itmFile: TMenuItem;
    itmFileOpen: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    itmApp: TMenuItem;
    itmHideMe: TMenuItem;
    actAppQuit: TFileExit;
    actAppHide: TFileHideApp;
    actAppHideOthers: TFileHideAppOthers;
    itmHideOtherApps: TMenuItem;
    itmAppQuit: TMenuItem;
    itmDiv: TMenuItem;
    itmFileClose: TMenuItem;
    actNextTab: TAction;
    actPreviousTab: TAction;
    itmWindow: TMenuItem;
    itmMinimize: TMenuItem;
    actZoomWindow: TAction;
    MenuItem1: TMenuItem;
    itmShowAll: TMenuItem;
    actAppShowAll: TAction;
    actBringAllToFront: TAction;
    MenuItem2: TMenuItem;
    itmBringAllToFront: TMenuItem;
    actAppAbout: TAction;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    actToggleFullScreen: TAction;
    actMinimizeWindow: TAction;
    procedure actOpenInDefProgExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actCopyTagsUpdate(Sender: TObject);
    procedure actCopyTagsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure actPreviousTabExecute(Sender: TObject);
    procedure actNextTabExecute(Sender: TObject);
    procedure actOpenInDefProgUpdate(Sender: TObject);
  strict private
    class var FDocumentFormClass: TFormClass;
    class var FSDIForm: TForm;
    class constructor Create;
  protected
    FClipboardService: IFMXClipboardService;
    procedure OpenFile(const AFileName: string);
  end;

var
  dtmController: TdtmController;

implementation

{%CLASSGROUP 'FMX.Types.TFmxObject'}

{$R *.dfm}

{ TdtmController }

class constructor TdtmController.Create;
begin
  if not LookupImplementingClass(IExifDocumentForm, TForm, FDocumentFormClass) then
    raise EProgrammerNotFound.Create('IExifDocumentForm implementor missing');
  if TOSVersion.Platform = pfMacOS then
    TFileManager.DocumentMode := dmOnePerForm
  else
  begin
    TFileManager.DocumentMode := dmOnePerAppInst;
    Application.CreateForm(FDocumentFormClass, FSDIForm);
  end;
end;

procedure TdtmController.DataModuleCreate(Sender: TObject);
begin
  { Retrieve a reference to the clipboard service, permanently disabling the Copy Tags
    action if there isn't one. }
  if not TPlatformService.Available<IFMXClipboardService>(FClipboardService) then
  begin
    actCopyTags.OnUpdate := nil;
    actCopyTags.Enabled := False;
  end;
  { Initialize the menu bar if targetting OS X }
  if TOSVersion.Platform = pfMacOS then
  begin
    TMacCommands.InitializeAboutAction(actAppAbout);
    TMacCommands.InitializeShowAllAction(actAppShowAll);
    TMacCommands.InitializeToggleFullScreenAction(actToggleFullScreen);
    TMacCommands.InitializeMinimizeAction(actMinimizeWindow);
    TMacCommands.InitializeZoomAction(actZoomWindow);
    TMacCommands.InitializeBringAllToFrontAction(actBringAllToFront);
    MacMenu.Activate;
    TMacCommands.InitializeWindowMenu(itmWindow);
  end
  else
    ConvertCommandToCtrlShortcuts(ActionList);
  TFileManager.OnOpenFile := OpenFile;
end;

procedure TdtmController.OpenFile(const AFileName: string);
var
  DocForm: TForm;
begin
  if TFileManager.DocumentMode = dmOnePerAppInst then
    DocForm := FSDIForm
  else
    DocForm := FDocumentFormClass.Create(Self);
  try
    (DocForm as IExifDocumentForm).OpenFile(AFileName);
  except
    if DocForm <> FSDIForm then DocForm.Free;
    raise;
  end;
  TFileManager.AssociateWithForm(AFileName, DocForm);
  DocForm.Show;
end;

procedure TdtmController.actCopyTagsExecute(Sender: TObject);
begin
  (Screen.ActiveForm as IExifDocumentForm).CopyTags(FClipboardService);
end;

procedure TdtmController.actCopyTagsUpdate(Sender: TObject);
var
  DocForm: IExifDocumentForm;
begin
  actCopyTags.Enabled := Supports(Screen.ActiveForm, IExifDocumentForm, DocForm) and
    DocForm.CanCopyTags;
end;

procedure TdtmController.actOpenFileExecute(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFile(dlgOpen.FileName);
end;

procedure TdtmController.actOpenInDefProgExecute(Sender: TObject);
begin
  TFileManager.ShellExecute((Screen.ActiveForm as IExifDocumentForm).FileName);
end;

procedure TdtmController.actOpenInDefProgUpdate(Sender: TObject);
var
  DocForm: IExifDocumentForm;
begin
  actOpenInDefProg.Enabled := Supports(Screen.ActiveForm, IExifDocumentForm, DocForm) and
    (DocForm.FileName <> '');
end;

procedure TdtmController.actNextTabExecute(Sender: TObject);
var
  DocForm: IExifDocumentForm;
begin
  if Supports(Screen.ActiveForm, IExifDocumentForm, DocForm) then
    DocForm.GotoNextTab;
end;

procedure TdtmController.actPreviousTabExecute(Sender: TObject);
var
  DocForm: IExifDocumentForm;
begin
  if Supports(Screen.ActiveForm, IExifDocumentForm, DocForm) then
    DocForm.GotoPreviousTab;
end;

end.
