{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.0 beta                                                                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is PanaMakerPatch.dpr.                                             }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

program PanaMakerPatch;

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows,
  SysUtils,
  Classes,
  XPMan,
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas';

procedure OutputInfo;
begin
  WriteLn('-----------------------------------------------------------');
  WriteLn('            Panasonic Maker Note Patcher v' + CCRExifVersion);
  WriteLn('-----------------------------------------------------------');
  WriteLn('');
  WriteLn('Usage:');
  WriteLn('PanaMakerPatch JpegFile TagID NewValue [/p]');
  WriteLn('');
  WriteLn('Notes:');
  WriteLn('- JpegFile can contain wildcards.');
  WriteLn('- TagID can be a decimal or hexadecimal number, and must');
  WriteLn('  denote a maker note tag that already exists in the source');
  WriteLn('  file(s). To specify more than one tag, delimit IDs with');
  WriteLn('  commas (don''t add spaces though).');
  WriteLn('- NewValue is the raw value as it can be represented in a');
  WriteLn('  string. For an array tag, use a comma to delimit element');
  WriteLn('  values.');
  WriteLn('- While the new number of elements for an array tag can be');
  WriteLn('  smaller than the old one, it cannot be greater.');
  WriteLn('  Similarly, when setting a string tag, the new value can');
  WriteLn('  be shorter than the old one, but it cannot be longer.');
  WriteLn('- Specify an empty string ("") to remove a tag''s data.');
  WriteLn('- If /p is specified, a source file''s ''date modified''');
  WriteLn('  value is preserved.');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('PanaMakerPatch P1000514.JPG $0033 "9999:99:99 00:00:00"');
  WriteLn('PanaMakerPatch *.jpg $0033,$8010 "2009:11:15 09:30:00" /p');
end;

procedure OutputProcessingError(const JpegFile, Msg: string);
begin
  Writeln('Problem processing ', JpegFile, ':');
  Writeln(Msg);
  Writeln('');
end;

var
  PreserveFileDates: Boolean;
  TagIDs: array of TExifTagID;
  NewValue: string;

procedure DoIt(const JpegFile: string);
var
  ExifData: TExifDataPatcher;
  ID: TExifTagID;
  Tag: TExifTag;
begin
  ExifData := TExifDataPatcher.Create(JpegFile);
  try
    ExifData.PreserveFileDate := PreserveFileDates;
    if not (ExifData.MakerNote is TPanasonicMakerNote) then
    begin
      OutputProcessingError(JpegFile, 'Panasonic maker note data not found');
      Exit;
    end;
    for ID in TagIDs do
      if ExifData[esMakerNote].Find(ID, Tag) then
        Tag.AsString := NewValue
      else
        OutputProcessingError(JpegFile, Format('Tag $%.4x not found', [ID]));
    if ExifData.Modified then
    begin
      ExifData.UpdateFile;
      Writeln('Updated ', JpegFile);
    end;
  finally
    ExifData.Free;
  end;
end;

var
  I, RetVal: Integer;
  S: string;
  SearchRec: TSearchRec;
begin
  try
    if ParamCount < 3 then
    begin
      if ParamCount = 0 then
        MessageBox(0, 'This is a console application that requires command-line arguments' +
          ' - use PanaMakerPatch /? for help on what they should be.',
          'Panasonic Maker Note Patcher v' + CCRExifVersion, MB_ICONINFORMATION)
      else
        OutputInfo;
      Exit;
    end;
    S := ParamStr(2);
    with TStringList.Create do
    try
      CommaText := S;
      SetLength(TagIDs, Count);
      for I := 0 to Count - 1 do
        if TryStrToInt(Strings[I], RetVal) and (RetVal >= Low(TExifTagID)) and (RetVal <= High(TExifTagID)) then
          TagIDs[I] := RetVal
        else
        begin
          Writeln('Invalid TagID value (', S, ')');
          Exit;
        end;
    finally
      Free;
    end;
    S := ParamStr(1);
    RetVal := FindFirst(S, faReadOnly or faArchive, SearchRec);
    if RetVal <> 0 then
    begin
      Writeln('Invalid source file name (', S, '):');
      Writeln(SysErrorMessage(RetVal));
      Exit;
    end;
    NewValue := ParamStr(3);
    PreserveFileDates := FindCmdLineSwitch('p', True);
    S := ExtractFilePath(S);
    repeat
      try
        DoIt(S + SearchRec.Name);
      except
        on E:Exception do
          OutputProcessingError(S + SearchRec.Name, E.Message)
        else
          raise;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  if DebugHook <> 0 then Readln;
end.
