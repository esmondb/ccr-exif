{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.1 beta                                                                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is FileTimeOptsForm.pas.                                           }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit FileTimeOptsForm;

interface

uses
  {$IF RTLVersion >= 23}System.UITypes,{$IFEND}
  SysUtils, Classes, Controls, Forms, StdCtrls, CCR.Exif.Demos;

type
  TFileTimeAction = (ftPreserve, ftMatchExif, ftSetToNow);

  TfrmFileTimeOptions = class(TForm)
    rdoPreserve: TRadioButton;
    rdoMatchExif: TRadioButton;
    rdoSetToNow: TRadioButton;
    Button1: TButton;
    Button2: TButton;
  public
    function ShowModal(var Action: TFileTimeAction): Boolean; reintroduce;
  end;

implementation

{$R *.dfm}

function TfrmFileTimeOptions.ShowModal(var Action: TFileTimeAction): Boolean;
begin
  case Action of
    ftPreserve: rdoPreserve.Checked := True;
    ftMatchExif: rdoMatchExif.Checked := True;
    ftSetToNow: rdoSetToNow.Checked := True;
  end;
  Result := IsPositiveResult(inherited ShowModal);
  if Result then
    if rdoPreserve.Checked then
      Action := ftPreserve
    else if rdoMatchExif.Checked then
      Action := ftMatchExif
    else
      Action := ftSetToNow;
end;

end.
