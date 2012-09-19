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

unit ImageForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  CCR.Exif, CCR.Exif.FMXUtils;

type
  TfrmImage = class(TForm)
    Image: TImage;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  protected
    procedure CreateHandle; override;
  public
    constructor Create(const AOwner: TExifDataPatcher); reintroduce;
    function Owner: TExifDataPatcher; inline;
  end;

implementation

{$R *.fmx}

constructor TfrmImage.Create(const AOwner: TExifDataPatcher);
begin
  inherited Create(AOwner);
  AOwner.GetImage(Image.Bitmap);
  case AOwner.Orientation of
    toBottomRight: Image.Bitmap.Rotate(180);
    toRightTop: Image.Bitmap.Rotate(90);
    toLeftBottom: Image.Bitmap.Rotate(270);
  end;
  TFileManager.AssociateWithForm(AOwner.FileName, Self);
  TMacCommands.EnableFullScreen(Self);
end;

procedure TfrmImage.CreateHandle;
begin
  inherited;
  TWinCommands.EnableOwnTaskbarEntry(Self);
end;

procedure TfrmImage.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkEscape, vkReturn:
    begin
      if TMacCommands.IsFullScreen(Self) then
        TMacCommands.ToggleFullScreen(Self)
      else
        Close;
      Key := 0;
    end;
  end;
end;

function TfrmImage.Owner: TExifDataPatcher;
begin
  Result := (inherited Owner as TExifDataPatcher);
end;

end.
