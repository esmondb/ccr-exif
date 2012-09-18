unit CCR.Exif.FMXUtils;
{
  This unit (a) surfaces some OS X functionality not exposed by FMX, (b) fixes some
  awful menu code in the FMX source, particularly for OS X, and (c) provides a
  custom NSApplication delegate that receives 'open file' messages. The latter can
  be handled by assigning the OnOpenFile event of the TFileManager singleton.
}
interface

{$IFDEF VER230}
{$MESSAGE ERROR 'Unit requires XE3, sorry'}
{$ENDIF}

uses
  System.Types, System.SysUtils, System.Classes, System.UITypes, System.Generics.Collections,
  System.TypInfo, System.Rtti, System.Actions, FMX.Types, FMX.Ani, FMX.Forms, FMX.Menus, FMX.ActnList, FMX.StdActns
  {$IFDEF MACOS}, Macapi.AppKit{$ENDIF};

type
  { Interposer classes to fix awful code in FMX.Menus.pas }

  TMenuItem = class(FMX.Menus.TMenuItem, IItemsContainer)
  private type
    TChangeKind = (ckChecked, ckEnabled, ckText, ckVisible); //unfortunately, IsChecked prop setter not virtual, so can't fix

    IFixedFMXMenuService = interface(IFMXMenuService)
    ['{DF5F5237-6640-4EFE-A5BB-C6AE7FC63459}']
      procedure UpdateMenuItemEx(const AItem: TMenuItem; AChange: TChangeKind);
    end;
  private
    class var FixedMenuService: IFixedFMXMenuService;
  strict private
    FIgnoreUpdateMenuItemCount: Integer;
    procedure SetIsChecked(const Value: Boolean);
  protected
    function IgnoreUpdateMenuItem: Boolean; inline;
    procedure Click; override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetText(const Value: string); override;
    procedure SetVisible(const Value: Boolean); override;
  published
    property IsChecked write SetIsChecked;
  end;

  { TMainMenu doesn't work if placed on a data module, and always activates itself
    if placed on a form. This is crap behaviour given the only platform where a native
    menu bar still makes 'native' sense is OS X, and OS X is where there is typically
    no 'main form' to 'own' the menu bar (instead, the application 'owns' it). The
    following interposer class therefore changes the default TMainMenu behaviour to
    require an explicit call to Activate. Oddly enough, this results in *less* code
    internally.

    Aside from that, this interposer also fixes the walking-up-the-list thing. }

  TMainMenu = class(FMX.Menus.TMainMenu, IItemsContainer)
  strict private
    FActive: Boolean;
    function GetMenuItem(const AIndex: Integer): TMenuItem;
  protected
    procedure DoAddObject(AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; AObject: TFmxObject); override;
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    procedure RecreateOSMenu; override;
    property Active: Boolean read FActive;
  public
    procedure Activate;
    property Items[const AIndex: Integer]: TMenuItem read GetMenuItem;
  end;

  { Interposer classes to fix over-fussy code in FMX.StdActns }

  TFileExit = class(FMX.StdActns.TFileExit)
  public
    procedure ExecuteTarget(Target: TObject); override;
    function Update: Boolean; override;
  end;

  TFileHideApp = class(FMX.StdActns.TFileHideApp)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function Update: Boolean; override;
  end;

  TFileHideAppOthers = class(FMX.StdActns.TFileHideAppOthers)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function Update: Boolean; override;
  end;

  TWindowClose = class(FMX.StdActns.TWindowClose)
  public
    function Update: Boolean; override;
  end;

  { Changes behaviour to work with action lists placed on data modules. NB: as I
    personally don't use them any other way, this implementation skips the default
    one's going through the action 'target' controls, and just asks the actions to
    update themselves directly. }

  TForm = class(FMX.Forms.TForm)
  strict private
    FInstanceCount: Integer;
  protected
    procedure UpdateActions; override;
  public
    procedure InitializeNewForm; override;
    destructor Destroy; override;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
  end;

  TFormClass = class of TForm;

  { Thin wrappers for some OS functionality not exposed by FMX }

  TOpenFileEvent = reference to procedure (const AFileName: string);

  TDocumentMode = (dmOnePerAppInst, dmOnePerForm, dmMultiplePerForm);

  TFileManager = record
  strict private
    class var FCheckedCmdLine: Boolean;
    class var FDocumentMode: TDocumentMode;
    class var FFilesToOpen: TList<string>;
    class var FOnOpenFile: TOpenFileEvent;
    class constructor InitializeClass;
    class destructor FinalizeClass;
    class procedure SetOnOpenFile(const AHandler: TOpenFileEvent); static;
  public
    class procedure AssociateWithForm(const AFileName: string; const AForm: TCommonCustomForm); static;
    class procedure OpenFile(const AFileName: string); static; inline;           //Raises OnOpenFile event
    class procedure OpenDroppedFiles(const AFileNames: array of string); static; //Behaviour depends on DocumentMode property
    class procedure ShellExecute(const AFileName: string); overload; static;
    class procedure ShellExecute(const AExecutableName: string; const Args: array of string); overload; static;
    class property DocumentMode: TDocumentMode read FDocumentMode write FDocumentMode;
    class property OnOpenFile: TOpenFileEvent read FOnOpenFile write SetOnOpenFile;
  end;

  {$IF NOT DECLARED(NSApplication)}  //dummy types to minimise IFDEF's later on...
  NSApplication = interface
    procedure arrangeInFront(sender: Pointer);
    procedure orderFrontStandardAboutPanel(sender: Pointer);
    procedure hide(sender: Pointer);
    procedure hideOtherApplications(sender: Pointer);
    procedure unhideAllApplications(sender: Pointer);
  end;

  NSWindow = Variant;
  {$ENDIF}

  TMacCommands = class
  strict private
    class var FCanEnableFullScreen: Boolean;
    class var FNSApp: NSApplication;
    class constructor Create;
    class function GetNSApp: NSApplication; static;
  public
    //Lion full screen mode support
    class property CanEnableFullScreen: Boolean read FCanEnableFullScreen;
    class procedure EnableFullScreen(const AForm: TCommonCustomForm); static;
    class function IsFullScreenEnabled(const AForm: TCommonCustomForm): Boolean; static;
    class function IsFullScreen(const AForm: TCommonCustomForm): Boolean; static;
    class procedure ToggleFullScreen(const AForm: TCommonCustomForm); overload; static;
    class function ToggleFullScreenText(AForm: TCommonCustomForm = nil): string; static;
    //the actual commands; the Sender parameter is so can directly assign as an action's OnExecute event handler
    class procedure About(Sender: TObject = nil);
    class procedure Hide(Sender: TObject = nil);
    class procedure HideOthers(Sender: TObject = nil);
    class procedure ShowAll(Sender: TObject = nil);
    class procedure Quit(Sender: TObject = nil);
    class procedure Close(Sender: TObject = nil);
    class procedure ToggleFullScreen(Sender: TObject = nil); overload;
    class procedure Minimize(Sender: TObject = nil);
    class procedure Zoom(Sender: TObject = nil);
    class procedure BringAllToFront(Sender: TObject = nil);
    //OnUpdate handlers
    class procedure UpdateToggleFullScreenAction(Sender: TObject);
    class procedure UpdateMinimizeAction(Sender: TObject);
    class procedure UpdateZoomAction(Sender: TObject);
    class procedure UpdateBringAllToFrontAction(Sender: TObject);
    //event assigners
    class procedure InitializeAboutAction(const Action: TCustomAction); static;
    class procedure InitializeShowAllAction(const Action: TCustomAction); static;
    class procedure InitializeToggleFullScreenAction(const Action: TCustomAction); static;
    class procedure InitializeMinimizeAction(const Action: TCustomAction); static;
    class procedure InitializeZoomAction(const Action: TCustomAction); static;
    class procedure InitializeBringAllToFrontAction(const Action: TCustomAction); static;
    //other
    class procedure InitializeWindowMenu(const ATopLevelItem: TMenuItem); static;
    class property NSApp: NSApplication read GetNSApp;
  end;

  TWinCommands = record
    class procedure EnableOwnTaskbarEntry(const AForm: TCommonCustomForm); static;
  end;

  TPlatformService = class
  strict private
    {$IFDEF MACOS}
    class var FAppDelegate: NSApplicationDelegate;
    {$ENDIF}
    class var FActionTimer: TFmxHandle;
    class var FTimerService: IFMXTimerService;
    class constructor Create;
  protected
    class procedure CreateActionTimer;
    class procedure DestroyActionTimer;
  public
    class function Available(const IntfType: TGUID): Boolean; overload; static; inline;
    class function Available<IntfType: IInterface>(out Service: IntfType): Boolean; overload; static; inline;
    class function Get<IntfType: IInterface>: IntfType; static; inline;
    class function Replace<IntfType: IInterface>(const Replacement: IntfType): IntfType; static;
    class procedure UpdateActionLists;
  end;

{ General helper routines}
procedure ConvertCommandToCtrlShortcuts(const ActionList: TContainedActionList);
function LookupImplementingClass(const AIntf: TGUID; const ABaseClassType: TClass;
  var AFoundClass): Boolean;
function LookupSelectedFontColor(AControl: TTextControl): TAlphaColor;

procedure Beep; //avoid stupid compiler hint caused by being inlined in SysUtils
procedure OutputDebugText(const AText: string); overload;
procedure OutputDebugText(const AFormatText: string; const Args: array of const); overload;

resourcestring
  SEnterFullScreen = 'Enter Full Screen';
  SExitFullScreen = 'Exit Full Screen';

implementation

uses
  System.StrUtils, System.Diagnostics, FMX.Platform,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellApi, FMX.Platform.Win
  {$ENDIF}
  {$IFDEF MACOS}
  Posix.Stdlib, Macapi.CoreFoundation, Macapi.ObjectiveC, Macapi.Foundation, FMX.Platform.Mac
  {$ENDIF};

type
  TPlatformServicesFix = class(TInterfacedObject, IFMXMenuService, TMenuItem.IFixedFMXMenuService)
  strict private
    FDefaultMenuService: IFMXMenuService;
    { IFMXMenuService }
    procedure CreateOSMenu(AForm: TCommonCustomForm;
      const AMenu: IItemsContainer);
    procedure DestroyMenuItem(const AItem: IItemsContainer);
    function IsMenuBarOnWindowBorder: Boolean;
    procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word;
      var Shift: TShiftState);
    function ShortCutToText(ShortCut: TShortCut): string;
    procedure StartMenuLoop(const AView: IMenuView);
    function TextToShortCut(Text: string): Integer;
    procedure UpdateMenuItem(const AItem: IItemsContainer);
    { IFixedFMXMenuService }
    procedure UpdateMenuItemEx(const AItem: TMenuItem; AChange: TMenuItem.TChangeKind);
  public
    constructor Create;
  end;

procedure ConvertCommandToCtrlShortcuts(const ActionList: TContainedActionList);
var
  Action: TContainedAction;
begin
  for Action in ActionList do
    if Action.ShortCut and scCommand <> 0 then
      Action.ShortCut := (Action.ShortCut and not scCommand) or scCtrl;
end;

function LookupImplementingClass(const AIntf: TGUID; const ABaseClassType: TClass;
  var AFoundClass): Boolean;
var
  Context: TRttiContext;
  LClass: TClass;
  LType: TRttiType;
begin
  for LType in Context.GetTypes do
    if LType is TRttiInstanceType then
    begin
      LClass := TRttiInstanceType(LType).MetaclassType;
      if LClass.InheritsFrom(ABaseClassType) and Supports(LClass, AIntf) then
      begin
        TClass(AFoundClass) := LClass;
        Exit(True);
      end;
    end;
  Result := False;
end;

function LookupSelectedFontColor(AControl: TTextControl): TAlphaColor;
var
  Obj: TFmxObject;
begin
  AControl.ApplyStyleLookup;
  Result := AControl.DefaultTextSettings.FontColor;
  Obj := AControl.FindStyleResource('text');
  if Obj = nil then Exit;
  for Obj in Obj.Children do
    if (Obj is TColorAnimation) and (TColorAnimation(Obj).PropertyName = 'Color') and
       (Pos('IsSelected', TColorAnimation(Obj).Trigger) > 0)  then
    begin
      Result := TColorAnimation(Obj).StopValue;
      Exit;
    end;
end;

{$HINTS OFF} //avoid braindead H2443
procedure Beep;
begin
  System.SysUtils.Beep;
end;
{$HINTS ON}

procedure OutputDebugText(const AText: string);
begin
{$IFDEF DEBUG}
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AText));
  {$ELSE}
  WriteLn(AText);
  {$ENDIF}
{$ENDIF}
end;

procedure OutputDebugText(const AFormatText: string; const Args: array of const);
begin
{$IFDEF DEBUG}
  OutputDebugText(Format(AFormatText, Args));
{$ENDIF}
end;


{$IF NOT DECLARED(NSWindowCollectionBehaviorFullScreenPrimary)}
const
  NSWindowCollectionBehaviorFullScreenPrimary = 128;
  NSFullScreenWindowMask = 16384;
{$IFEND}

{$IFNDEF MACOS}
procedure RaisePlatformUnsupportedError;
begin
  raise ENotSupportedException.Create('Platform not supported');
end;

function GetNSWindow(const AForm: TCommonCustomForm): NSWindow;
begin
  RaisePlatformUnsupportedError;
end;

function GetNSWindowLion(Form: TCommonCustomForm): NSWindow;
begin
  RaisePlatformUnsupportedError;
end;

{$ELSE}

type
  {$M+}
  NSApplicationDelegateEx = interface(NSApplicationDelegate)
    ['{BE9AEDB7-80AC-49B1-8921-F226CC9310F4}']
    function application(theApplication: Pointer; openFile: CFStringRef): Boolean; cdecl;
  end;

  TNSApplicationDelegateEx = class(TOCLocal, NSApplicationDelegate, NSApplicationDelegateEx)
  public
    constructor Create;
    procedure applicationDidFinishLaunching(Notification: Pointer); cdecl;
    procedure applicationWillTerminate(Notification: Pointer); cdecl;
    function application(theApplication: Pointer; openFile: CFStringRef): Boolean; cdecl;
  end;
  {$M-}

  NSWindowLion = interface(NSWindow)
  ['{7AED4558-D490-4450-8CBA-4A6FC1796C25}']
    procedure toggleFullScreen(sender: Pointer = nil); cdecl;
  end;

  TNSWindowLion = class(TOCGenericImport<NSWindowClass, NSWindowLion>);

function GetNSWindow(const AForm: TCommonCustomForm): NSWindow; inline;
begin
  Result := NSWindow(NSWindowFromObjC(FmxHandleToObjC(AForm.Handle)));
end;

function GetNSWindowLion(Form: TCommonCustomForm): NSWindowLion; inline;
begin
  Result := TNSWindowLion.Wrap((FmxHandleToObjC(Form.Handle) as ILocalObject).GetObjectID);
end;

function NSSTR(const S: string): NSString;
var
  Ref: CFStringRef;
begin
  Ref := CFStringCreateWithCharacters(nil, PChar(S), Length(S));
  TNSAutoreleasePool.OCClass.addObject(Ref);
  Result := TNSString.Wrap(Ref)
end;

{ TNSApplicationDelegateEx }

constructor TNSApplicationDelegateEx.Create;
begin
  inherited Create;
  TMacCommands.NSApp.setDelegate(Self);
end;

function TNSApplicationDelegateEx.application(theApplication: Pointer;
  openFile: CFStringRef): Boolean;
var
  Range: CFRange;
  S: string;
begin
  Result := True;
  Range.location := 0;
  Range.length := CFStringGetLength(openFile);
  SetLength(S, Range.length);
  CFStringGetCharacters(openFile, Range, PChar(S));
  try
    TFileManager.OpenFile(S);
  except
    FMX.Forms.Application.HandleException(ExceptObject);
    Result := False;
  end;
end;

procedure TNSApplicationDelegateEx.applicationDidFinishLaunching(
  Notification: Pointer);
begin
  { The default FMX delegate doesn't do anything here, so nor will we. }
end;

procedure TNSApplicationDelegateEx.applicationWillTerminate(
  Notification: Pointer);
begin
  FMX.Forms.Application.Free;
  FMX.Forms.Application := nil;
end;
{$ENDIF}

{ TPlatformServicesFix }

constructor TPlatformServicesFix.Create;
begin
  inherited Create;
  FDefaultMenuService := TPlatformService.Replace<IFMXMenuService>(Self);
  TMenuItem.FixedMenuService := Self;
end;

procedure TPlatformServicesFix.CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
var
  Obj: TMainMenu;
begin
  Obj := TMainMenu(AMenu);
  if (Obj = nil) or Obj.Active then
    FDefaultMenuService.CreateOSMenu(AForm, AMenu);
end;

procedure TPlatformServicesFix.DestroyMenuItem(const AItem: IItemsContainer);
begin
  FDefaultMenuService.DestroyMenuItem(AItem);
end;

function TPlatformServicesFix.IsMenuBarOnWindowBorder: Boolean;
begin
  Result := FDefaultMenuService.IsMenuBarOnWindowBorder
end;

procedure TPlatformServicesFix.ShortCutToKey(ShortCut: TShortCut; var Key: Word;
  var Shift: TShiftState);
begin
  FDefaultMenuService.ShortCutToKey(ShortCut, Key, Shift);
end;

function TPlatformServicesFix.ShortCutToText(ShortCut: TShortCut): string;
begin
  Result := FDefaultMenuService.ShortCutToText(ShortCut);
end;

procedure TPlatformServicesFix.StartMenuLoop(const AView: IMenuView);
begin
  FDefaultMenuService.StartMenuLoop(AView);
end;

function TPlatformServicesFix.TextToShortCut(Text: string): Integer;
begin
  Result := FDefaultMenuService.TextToShortCut(Text);
end;

procedure TPlatformServicesFix.UpdateMenuItem(const AItem: IItemsContainer);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem(AItem);
  if (MenuItem = nil) or not MenuItem.IgnoreUpdateMenuItem then
    FDefaultMenuService.UpdateMenuItem(AItem);
end;

procedure TPlatformServicesFix.UpdateMenuItemEx(const AItem: TMenuItem; AChange: TMenuItem.TChangeKind);
var
  Handle: TFmxHandle;
  Parent: TFmxObject;
  {$IFDEF MACOS}
  NSObj: NSMenuItem;
  {$ELSE}
  ParentHandle: HMENU;
  {$ENDIF}
begin
  //don't do anything if on a non-active main menu
  Parent := AItem.Parent;
  while (Parent <> nil) and not (Parent is TMainMenu) do
    Parent := Parent.Parent;
  if (Parent is TMainMenu) and not TMainMenu(Parent).Active then Exit;
  //do the stock behaviour if not a native menu item
  Handle := AItem.Handle;
  if Handle = 0 then
  begin
    FDefaultMenuService.UpdateMenuItem(AItem);
    Exit;
  end;
  {$IFDEF MACOS}
  NSObj := TNSMenuItem.Wrap((FmxHandleToObjC(Handle) as ILocalObject).GetObjectID);
  case AChange of
    ckChecked:
      if AItem.IsChecked then
        NSObj.setState(NSOnState)
      else
        NSObj.setState(NSOffState);
    ckEnabled: NSObj.setEnabled(AItem.Enabled);
    ckText: NSObj.setTitle(NSSTR(AItem.Text));
    ckVisible: NSObj.setHidden(not AItem.Visible);
  end;
  {$ELSE}
  if (AChange = ckVisible) or not (AItem.Parent is TMenuItem) then
  begin
    FDefaultMenuService.UpdateMenuItem(AItem);
    Exit;
  end;
  ParentHandle := TMenuItem(AItem.Parent).Handle;
  case AChange of
    ckChecked:
      if AItem.IsChecked then
        ModifyMenu(ParentHandle, AItem.Handle, MF_BYCOMMAND or MF_CHECKED, AItem.Handle, nil)
      else
        ModifyMenu(ParentHandle, AItem.Handle, MF_BYCOMMAND or MF_UNCHECKED, AItem.Handle, nil);
    ckEnabled:
      if AItem.Enabled then
        EnableMenuItem(ParentHandle, AItem.Handle, MF_BYCOMMAND or MF_ENABLED)
      else
        EnableMenuItem(ParentHandle, AItem.Handle, MF_BYCOMMAND or MF_GRAYED);
    ckText: ModifyMenu(ParentHandle, AItem.Handle, MF_BYCOMMAND or MF_STRING,
      AItem.Handle, PChar(AItem.Text))
  end;
  {$ENDIF}
end;

{ TFileManager }

class constructor TFileManager.InitializeClass;
begin
  FFilesToOpen := TList<string>.Create;
end;

class destructor TFileManager.FinalizeClass;
begin
  FFilesToOpen.Free;
end;

class procedure TFileManager.SetOnOpenFile(const AHandler: TOpenFileEvent);
var
  I: Integer;
  S: string;
begin
  if not FCheckedCmdLine and Assigned(AHandler) then
  begin
    FCheckedCmdLine := True;
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if (S <> '') and not CharInSet(S[1], SwitchChars) then
      begin
        FFilesToOpen.Add(S);
        if DocumentMode = dmOnePerAppInst then Break;
      end;
    end;
  end;
  FOnOpenFile := AHandler;
  if Assigned(AHandler) then
    try
      for S in FFilesToOpen do
        try
          AHandler(S);
        except
          if Application <> nil then
            Application.HandleException(ExceptObject)
          else
            ShowException(ExceptObject, ExceptAddr);
        end;
    finally
      FFilesToOpen.Clear;
    end;
end;

class procedure TFileManager.AssociateWithForm(const AFileName: string; const AForm: TCommonCustomForm);
begin
  {$IFDEF MACOS}
  GetNSWindow(AForm).setTitleWithRepresentedFilename(NSSTR(AFileName));
  {$ELSE}
  AForm.Caption := ExtractFileName(AFileName) + ' - ' + Application.Title;
  {$ENDIF}
end;

class procedure TFileManager.OpenFile(const AFileName: string);
begin
  if AFileName <> '' then
    if Assigned(FOnOpenFile) then
      FOnOpenFile(AFileName)
    else
      FFilesToOpen.Add(AFileName);
end;

class procedure TFileManager.OpenDroppedFiles(const AFileNames: array of string);
var
  ExePath: string;
  I: Integer;
begin
  if DocumentMode = dmOnePerAppInst then
  begin
    OpenFile(AFileNames[0]);
    ExePath := GetModuleName(0);
    for I := 1 to High(AFileNames) do
      TFileManager.ShellExecute(ExePath, AFileNames[I])
  end
  else
    for I := 0 to High(AFileNames) do
      TFileManager.OpenFile(AFileNames[I])
end;

function EscapeForPosix(const AFileName: string): string; inline;
begin
  {$IFDEF POSIX}
  Result := AFileName.Replace('\', '\\').Replace('"', '\"');
  {$ELSE}
  Result := AFileName;
  {$ENDIF}
end;

class procedure TFileManager.ShellExecute(const AFileName: string);
begin
  {$IFDEF MSWINDOWS}
  Winapi.ShellApi.ShellExecute(0, nil, PChar(AFileName), nil, nil, SW_SHOWNORMAL)
  {$ELSE}
  _system(PAnsiChar(UTF8String('open "' + EscapeForPosix(AFileName) + '"')));
  {$ENDIF}
end;

class procedure TFileManager.ShellExecute(const AExecutableName: string; const Args: array of string);
var
  Arg, ArgsStr: string;
begin
  for Arg in Args do
    ArgsStr := ArgsStr + '"' + EscapeForPosix(Arg) + '" ';
  {$IFDEF MSWINDOWS}
  Winapi.ShellApi.ShellExecute(0, nil, PChar(AExecutableName), Pointer(ArgsStr), nil, SW_SHOWNORMAL)
  {$ELSE}
  _system(PAnsiChar(UTF8String('open "' + EscapeForPosix(AExecutableName) + '" ' + ArgsStr)));
  {$ENDIF}
end;

{ TMacCommands }

class constructor TMacCommands.Create;
begin
  FCanEnableFullScreen := (TOSVersion.Platform = pfMacOS) and TOSVersion.Check(10, 7);
end;

class function TMacCommands.GetNSApp: NSApplication;
begin
  {$IFDEF MACOS}
  if FNSApp = nil then FNSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
  {$ELSE}
  RaisePlatformUnsupportedError;
  {$ENDIF}
  Result := FNSApp;
end;

class procedure TMacCommands.InitializeWindowMenu(const ATopLevelItem: TMenuItem);
{$IFDEF MACOS}
var
  Menu: NSMenu;
  MenuItem: NSMenuItem;
begin
  Menu := TMacCommands.NSApp.mainMenu;
  if Menu = nil then
    raise EProgrammerNotFound.Create('OS menu not set up yet');
  MenuItem := FmxHandleToObjC(ATopLevelItem.Handle) as NSMenuItem;
  TMacCommands.NSApp.setWindowsMenu(TNSMenu.Wrap(MenuItem.submenu)); //!!!submenu result type no longer wrapped automatically in XE3
end;
{$ELSE}
begin
  RaisePlatformUnsupportedError;
end;
{$ENDIF}

class procedure TMacCommands.EnableFullScreen(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
begin
  if not CanEnableFullScreen then Exit;
  NSWin := GetNSWindow(AForm);
  NSWin.setCollectionBehavior(NSWin.collectionBehavior or
    NSWindowCollectionBehaviorFullScreenPrimary);
end;

class function TMacCommands.IsFullScreenEnabled(const AForm: TCommonCustomForm): Boolean;
begin
  Result := CanEnableFullScreen and (AForm <> nil) and (AForm.Handle <> 0) and
   (GetNSWindow(AForm).collectionBehavior and NSWindowCollectionBehaviorFullScreenPrimary <> 0);
end;

class function TMacCommands.IsFullScreen(const AForm: TCommonCustomForm): Boolean;
begin
  Result := CanEnableFullScreen and (AForm <> nil) and (AForm.Handle <> 0) and
    (GetNSWindow(AForm).styleMask and NSFullScreenWindowMask <> 0);
end;

class procedure TMacCommands.ToggleFullScreen(const AForm: TCommonCustomForm);
begin
  if CanEnableFullScreen and (AForm <> nil) and (AForm.Handle <> 0) then
    GetNSWindowLion(AForm).toggleFullScreen;
end;

class function TMacCommands.ToggleFullScreenText(AForm: TCommonCustomForm = nil): string;
begin
  if AForm = nil then AForm := Screen.ActiveForm;
  if IsFullScreen(AForm) then
    Result := SExitFullScreen
  else
    Result := SEnterFullScreen;
end;

class procedure TMacCommands.About;
begin
  NSApp.orderFrontStandardAboutPanel(nil);
end;

class procedure TMacCommands.Hide;
begin
  NSApp.hide(nil);
end;

class procedure TMacCommands.HideOthers;
begin
  NSApp.hideOtherApplications(nil);
end;

class procedure TMacCommands.ShowAll;
begin
  NSApp.unhideAllApplications(nil);
end;

class procedure TMacCommands.Quit;
begin
  if (Application = nil) or Application.Terminated then Exit;
  {$IFDEF MACOS}
  Application.Terminated := True; //doesn't appear ever set to True on OS X!
  {$ENDIF}
  Application.Terminate;
end;

class procedure TMacCommands.Close;
var
  Form: TCommonCustomForm;
begin
  if Sender is TCommonCustomForm then
    Form := TCommonCustomForm(Sender)
  else
  begin
    Form := Screen.ActiveForm;
    if Form = nil then Exit;
  end;
  if IsFullScreen(Form) then
    ToggleFullScreen(Form);
  Form.Close;
end;

class procedure TMacCommands.ToggleFullScreen(Sender: TObject = nil);
begin
  if not (Sender is TCommonCustomForm) then
  begin
    Sender := Screen.ActiveForm;
    if Sender = nil then Exit;
  end;
  ToggleFullScreen(TCommonCustomForm(Sender));
end;

class procedure TMacCommands.Minimize;
begin
  if Screen.ActiveForm <> nil then
    Screen.ActiveForm.WindowState := TWindowState.wsMinimized;
end;

class procedure TMacCommands.Zoom;
begin
  if Screen.ActiveForm = nil then Exit;
  if Screen.ActiveForm.WindowState = TWindowState.wsMaximized then
    Screen.ActiveForm.WindowState := TWindowState.wsNormal
  else
    Screen.ActiveForm.WindowState := TWindowState.wsMaximized;
end;

class procedure TMacCommands.BringAllToFront;
begin
  NSApp.arrangeInFront(nil);
end;

class procedure TMacCommands.UpdateToggleFullScreenAction(Sender: TObject);
var
  Action: TCustomAction;
  Form: TCommonCustomForm;
begin
  Action := (Sender as TCustomAction);
  Form := Screen.ActiveForm;
  Action.Enabled := IsFullScreenEnabled(Form);
  Action.Text := ToggleFullScreenText(Form);
end;

class procedure TMacCommands.UpdateMinimizeAction(Sender: TObject);
var
  Form: TCommonCustomForm;
begin
  Form := Screen.ActiveForm;
  (Sender as TCustomAction).Enabled := (Form <> nil) and (TBorderIcon.biMinimize in Form.BorderIcons) and
    not IsFullScreen(Form);
end;

class procedure TMacCommands.UpdateZoomAction(Sender: TObject);
var
  Form: TCommonCustomForm;
begin
  Form := Screen.ActiveForm;
  (Sender as TCustomAction).Enabled := (Form <> nil) and (TBorderIcon.biMaximize in Form.BorderIcons) and
    not IsFullScreen(Form);
end;

class procedure TMacCommands.UpdateBringAllToFrontAction(Sender: TObject);
begin
  (Sender as TCustomAction).Enabled := (Screen.FormCount > 0);
end;

class procedure TMacCommands.InitializeAboutAction(const Action: TCustomAction);
begin
  Action.OnExecute := About;
end;

class procedure TMacCommands.InitializeShowAllAction(const Action: TCustomAction);
begin
  Action.OnExecute := ShowAll;
end;

class procedure TMacCommands.InitializeToggleFullScreenAction(const Action: TCustomAction);
begin
  Action.Enabled := CanEnableFullScreen;
  if CanEnableFullScreen then
  begin
    Action.OnExecute := ToggleFullScreen;
    Action.OnUpdate := UpdateToggleFullScreenAction;
  end;
end;

class procedure TMacCommands.InitializeMinimizeAction(const Action: TCustomAction);
begin
  Action.OnExecute := Minimize;
  Action.OnUpdate := UpdateMinimizeAction;
end;

class procedure TMacCommands.InitializeZoomAction(const Action: TCustomAction);
begin
  Action.OnExecute := Zoom;
  Action.OnUpdate := UpdateZoomAction;
end;

class procedure TMacCommands.InitializeBringAllToFrontAction(const Action: TCustomAction);
begin
  Action.OnExecute := BringAllToFront;
  Action.OnUpdate := UpdateBringAllToFrontAction;
end;

{ TMenuItem interposer class }

procedure TMenuItem.Click;
var
  DoIgnore: Boolean;
begin
  DoIgnore := AutoCheck;
  if DoIgnore then Inc(FIgnoreUpdateMenuItemCount);
  try
    inherited;
    if DoIgnore then FixedMenuService.UpdateMenuItemEx(Self, ckChecked);
  finally
    if DoIgnore then Dec(FIgnoreUpdateMenuItemCount);
  end;
end;

function TMenuItem.IgnoreUpdateMenuItem: Boolean;
begin
  Result := (FIgnoreUpdateMenuItemCount > 0);
end;

procedure TMenuItem.SetEnabled(const Value: Boolean);
begin
  if Value = Enabled then Exit;
  Inc(FIgnoreUpdateMenuItemCount);
  try
    inherited;
  finally
    Dec(FIgnoreUpdateMenuItemCount);
  end;
  FixedMenuService.UpdateMenuItemEx(Self, ckEnabled);
end;

procedure TMenuItem.SetIsChecked(const Value: Boolean);
begin
  if Value = IsChecked then Exit;
  Inc(FIgnoreUpdateMenuItemCount);
  try
    inherited IsChecked := Value;
  finally
    Dec(FIgnoreUpdateMenuItemCount);
  end;
  FixedMenuService.UpdateMenuItemEx(Self, ckChecked);
end;

procedure TMenuItem.SetText(const Value: string);
begin
  if Value = Text then Exit;
  Inc(FIgnoreUpdateMenuItemCount);
  try
    inherited;
  finally
    Dec(FIgnoreUpdateMenuItemCount);
  end;
  FixedMenuService.UpdateMenuItemEx(Self, ckText);
end;

procedure TMenuItem.SetVisible(const Value: Boolean);
begin
  if Value = Visible then Exit;
  Inc(FIgnoreUpdateMenuItemCount);
  try
    inherited;
  finally
    Dec(FIgnoreUpdateMenuItemCount);
  end;
  FixedMenuService.UpdateMenuItemEx(Self, ckVisible);
end;

{ TMainMenu interposer class }

procedure TMainMenu.DoAddObject(AObject: TFmxObject);
begin
  if not (AObject is TMenuItem) then
    raise EInvalidInsert.Create('TMainMenu can only accept TMenuItem children');
  inherited;
end;

procedure TMainMenu.DoInsertObject(Index: Integer; AObject: TFmxObject);
begin
  if not (AObject is TMenuItem) then
    raise EInvalidInsert.Create('TMainMenu can only accept TMenuItem children');
  inherited;
end;

function TMainMenu.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Children[AIndex];
end;

function TMainMenu.GetItemsCount: Integer;
begin
  Result := ChildrenCount;
end;

function TMainMenu.GetMenuItem(const AIndex: Integer): TMenuItem;
begin
  Result := Children[AIndex] as TMenuItem;
end;

procedure TMainMenu.Activate;
begin
  FActive := True;
  try
    TPlatformService.Get<IFMXMenuService>.CreateOSMenu(Application.MainForm, Self);
  except
    FActive := False;
    raise;
  end;
end;

procedure TMainMenu.RecreateOSMenu;
begin
  { do nothing - Activate method now controls this }
end;

{ TFileExit }

function DoUpdate(const Action: TSysCommonAction): Boolean;
begin
  //why on earth does the default implementation constantly call CustomTextChanged?
  if not Action.Supported then Exit(False);
  Result := ((Action.ActionList <> nil) and Action.ActionList.UpdateAction(Action)) or
             (Application.UpdateAction(Action));
  if not Result and Assigned(Action.OnUpdate) then
  begin
    Action.OnUpdate(Action);
    Result := True;
  end;
end;

procedure TFileExit.ExecuteTarget(Target: TObject);
begin
  TMacCommands.Quit;
end;

function TFileExit.Update: Boolean;
begin
  Result := DoUpdate(Self);
end;

{ TFileHideApp }

constructor TFileHideApp.Create(AOwner: TComponent);
begin
  inherited;
  UpdateSupported;
end;

procedure TFileHideApp.ExecuteTarget(Target: TObject);
begin
  TMacCommands.Hide;
end;

function TFileHideApp.Update: Boolean;
begin
  Result := DoUpdate(Self);
end;

{ TFileHideAppOthers }

constructor TFileHideAppOthers.Create(AOwner: TComponent);
begin
  inherited;
  UpdateSupported;
end;

procedure TFileHideAppOthers.ExecuteTarget(Target: TObject);
begin
  TMacCommands.HideOthers;
end;

function TFileHideAppOthers.Update: Boolean;
begin
  Result := DoUpdate(Self);
end;

{ TWindowClose }

function TWindowClose.Update: Boolean;
var
  Form: TCommonCustomForm;
begin
  Result := DoUpdate(Self);
  if Result then Exit;
  if Screen = nil then
    Enabled := False
  else
  begin
    Form := Screen.ActiveForm;
    Enabled := (Form <> nil) and not (TFmxFormState.fsModal in Form.FormState) and
      not TMacCommands.IsFullScreen(Form)
  end;
end;

{ TForm }

procedure TForm.InitializeNewForm;
begin
  Inc(FInstanceCount);
  if FInstanceCount = 1 then TPlatformService.DestroyActionTimer;
  inherited;
end;

destructor TForm.Destroy;
begin
  Dec(FInstanceCount);
  if (FInstanceCount = 0) and (Application <> nil) and not Application.Terminated then
    TPlatformService.CreateActionTimer;
  inherited;
end;

procedure TForm.KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  I: Integer;
  Comp: TComponent;
  Module: TDataModule;
begin
  if (Shift * [ssAlt, ssCtrl, ssCommand] <> []) or (KeyChar < ' ') then
    for I := 0 to Screen.DataModuleCount - 1 do
    begin
      Module := Screen.DataModules[I];
      for Comp in Module do
        if (Comp is TCustomActionList) and TCustomActionList(Comp).DialogKey(Key, Shift) then
          Exit;
    end;
  inherited;
end;

procedure TForm.UpdateActions;
begin
  if Active then TPlatformService.UpdateActionLists;
end;

{ TWinCommands }

class procedure TWinCommands.EnableOwnTaskbarEntry(const AForm: TCommonCustomForm);
begin
  {$IFDEF MSWINDOWS}
  if (Screen.FormCount <> 0) and (AForm <> Screen.Forms[0]) then
    SetWindowLongPtr(FmxHandleToHWND(AForm.Handle), GWL_HWNDPARENT, 0);
  {$ENDIF}
end;

{ TPlatformService }

class constructor TPlatformService.Create;
var
  FixedImpls: IInterface;
begin
  FixedImpls := TPlatformServicesFix.Create;
  {$IFDEF MACOS}
  FAppDelegate := TNSApplicationDelegateEx.Create;
  {$ENDIF}
  FTimerService := Get<IFMXTimerService>;
  CreateActionTimer;
end;

class procedure TPlatformService.CreateActionTimer;
begin
  if FActionTimer <> 0 then Exit;
  FActionTimer := FTimerService.CreateTimer(150, UpdateActionLists);
end;

class procedure TPlatformService.DestroyActionTimer;
begin
  if FActionTimer = 0 then Exit;
  FTimerService.DestroyTimer(FActionTimer);
  FActionTimer := 0;
end;

class procedure TPlatformService.UpdateActionLists;

  procedure ProcessActionLists(const AOwner: TComponent);
  var
    Action: TContainedAction;
    Comp: TComponent;
  begin
    for Comp in AOwner do
      if Comp is TContainedActionList then
        if TContainedActionList(Comp).State = asNormal then
          for Action in TContainedActionList(Comp) do
            Action.Update;
  end;
var
  I: Integer;
begin
  if Screen = nil then Exit;
  if (Application <> nil) and Application.Terminated then
  begin
    DestroyActionTimer;
    Exit;
  end;
  for I := 0 to Screen.DataModuleCount - 1 do
    ProcessActionLists(Screen.DataModules[I]);
  for I := 0 to Screen.FormCount - 1 do
    ProcessActionLists(Screen.Forms[I]);
end;

class function TPlatformService.Available(const IntfType: TGUID): Boolean;
begin
  Result := TPlatformServices.Current.SupportsPlatformService(IntfType)
end;

class function TPlatformService.Available<IntfType>(out Service: IntfType): Boolean;
var
  Guid: TGUID;
begin
  Guid := PTypeInfo(TypeInfo(IntfType)).TypeData.Guid;
  Result := TPlatformServices.Current.SupportsPlatformService(Guid, IInterface(Service));
end;

class function TPlatformService.Get<IntfType>: IntfType;
var
  Guid: TGUID;
begin
  Guid := PTypeInfo(TypeInfo(IntfType)).TypeData.Guid;
  Result := IntfType(TPlatformServices.Current.GetPlatformService(Guid));
end;

class function TPlatformService.Replace<IntfType>(const Replacement: IntfType): IntfType;
var
  Guid: TGUID;
begin
  Guid := PTypeInfo(TypeInfo(IntfType)).TypeData.Guid;
  Result := IntfType(TPlatformServices.Current.GetPlatformService(Guid));
  TPlatformServices.Current.RemovePlatformService(Guid);
  TPlatformServices.Current.AddPlatformService(Guid, Replacement);
end;

end.
