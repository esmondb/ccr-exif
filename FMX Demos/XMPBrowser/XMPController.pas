{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.2                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit XMPController;

interface

uses
  System.Types, System.SysUtils, System.Classes, System.Rtti, FMX.Types, FMX.Forms,
  System.Actions, FMX.ActnList, FMX.StdActns, FMX.Dialogs, FMX.Menus,
  CCR.Exif.FMXUtils;

type
  IXMPDocumentForm = interface
  ['{5E1754C3-6F61-40B3-9840-CB951A31298E}']
    function GetFileName: string;
    procedure GotoNextTab;
    procedure GotoPreviousTab;
    procedure OpenFile(const AFileName: string);
    property FileName: string read GetFileName;
  end;

  TdtmController = class(TDataModule)
    MacMenu: TMainMenu;
    itmApp: TMenuItem;
    MenuItem5: TMenuItem;
    itmDiv: TMenuItem;
    itmHideMe: TMenuItem;
    itmHideOtherApps: TMenuItem;
    itmShowAll: TMenuItem;
    MenuItem6: TMenuItem;
    itmAppQuit: TMenuItem;
    itmFile: TMenuItem;
    itmFileOpen: TMenuItem;
    itmFileClose: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    itmWindow: TMenuItem;
    itmMinimize: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    itmBringAllToFront: TMenuItem;
    dlgOpen: TOpenDialog;
    ActionList: TActionList;
    actOpenFile: TAction;
    actClose: TWindowClose;
    actAppQuit: TFileExit;
    actAppHide: TFileHideApp;
    actAppHideOthers: TFileHideAppOthers;
    actNextTab: TAction;
    actPreviousTab: TAction;
    actMinimizeWindow: TAction;
    actZoomWindow: TAction;
    actAppShowAll: TAction;
    actBringAllToFront: TAction;
    actAppAbout: TAction;
    actToggleFullScreen: TAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
  private
  strict private
    class var FDocumentFormClass: TFormClass;
    class var FSDIForm: TForm;
    class constructor Create;
  protected
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
  if not LookupImplementingClass(IXMPDocumentForm, TForm, FDocumentFormClass) then
    raise EProgrammerNotFound.Create('IXMPDocumentForm implementor missing');
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
    (DocForm as IXMPDocumentForm).OpenFile(AFileName);
  except
    if DocForm <> FSDIForm then DocForm.Free;
    raise;
  end;
  TFileManager.AssociateWithForm(AFileName, DocForm);
  DocForm.Show;
end;

procedure TdtmController.actOpenFileExecute(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFile(dlgOpen.FileName);
end;

end.
