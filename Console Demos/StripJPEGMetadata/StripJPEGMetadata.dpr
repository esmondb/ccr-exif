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
{ The Original Code is StripJPEGMetadata.dpr.                                          }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

program StripJPEGMetadata;

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows,
  SysUtils,
  Classes,
  StrUtils,
  XPMan,
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas';

procedure OutputInfo;
begin
  WriteLn('----------------------------------------------------------------------------');
  WriteLn('                     JPEG Metadata Stripper v' + CCRExifVersion);
  WriteLn('----------------------------------------------------------------------------');
  WriteLn('');
  WriteLn('Usage:');
  WriteLn('StripJPEGMetadata Jpeg1 [Jpeg2 Jpeg3...] [switches]');
  WriteLn('');
  WriteLn('The file name(s) may contain wildcards.');
  WriteLn('');
  WriteLn('Switches:');
  WriteLn('/all     Remove all APP and COM segments, excepting any JFIF header. This is');
  WriteLn('         the default if no specific segments or metadata kinds are specfied.');
  WriteLn('/app#    Remove any APP# segements, where # is a number between 0 and 15');
  WriteLn('         inclusive.');
  WriteLn('/com     Remove any JPEG comment (i.e., any COM segment).');
  WriteLn('/exif    Remove any Exif data.');
  WriteLn('/iptc    Remove any IPTC data, as stored in a APP13 segment. Any non-IPTC');
  WriteLn('         data in the latter will be preserved.');
  WriteLn('/xmp     Remove any XMP packets.');
  WriteLn('/backup  If a file of the form "MyFile (original).jpg" does not already');
  WriteLn('         exist, it is created by copying the source before any metadata is');
  WriteLn('         removed.');
  WriteLn('/quiet   Causes the program not to report what has been removed.');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('StripJPEGMetadata "My Photo.jpeg"');
  WriteLn('StripJPEGMetadata "Another Photo.jpg" /app2 /app13');
  WriteLn('StripJPEGMetadata *.jpg *.jpeg /xmp /backup');
end;

procedure OutputProcessingError(const JpegFile, Msg: string);
begin
  Writeln('Problem processing ', JpegFile, ':');
  Writeln(Msg);
  Writeln('');
end;

var
  MakeBackup: Boolean = False;
  StripKinds: TJPEGMetadataKinds = [];
  StripSegments: TJPEGMarkers = [];
  Quiet: Boolean = False;

procedure ProcessFile(const JpegFile: string);
var
  BackupFN: string;
  RemovedKinds: TJPEGMetadataKinds;
  RemovedSegments: TJPEGMarkers;
begin
  if MakeBackup then
  begin
    BackupFN := ChangeFileExt(JpegFile, '') + ' (original)' + ExtractFileExt(JpegFile);
    if not FileExists(BackupFN) and not CopyFile(PChar(JpegFile), PChar(BackupFN), False) then
    begin
      OutputProcessingError(JpegFile, SysErrorMessage(GetLastError));
      Exit;
    end;
  end;
  RemovedKinds := RemoveMetadataFromJPEG(JpegFile, StripKinds); //defined and implemented in CCR.Exif.pas
  RemovedSegments := RemoveJPEGSegments(JpegFile, StripSegments);  //defined and implemented in CCR.Exif.BaseUtils.pas
  if not Quiet then
    if (RemovedKinds <> []) and (RemovedSegments <> []) then
      Writeln('Removed metadata from ', JpegFile)
    else
      Writeln('No metadata to remove from ', JpegFile);
end;

procedure DoIt;
var
  Files: TStringList;
  I, RetVal: Integer;
  S: string;
  SearchRec: TSearchRec;
begin
  Files := TStringList.Create;
  try
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if S = '' then Continue;
      if not (AnsiChar(S[1]) in SwitchChars) then
        Files.Add(S)
      else
      begin
        S := LowerCase(Copy(S, 2, MaxInt));
        if S = '' then Continue;
        if S = 'all' then
          StripSegments := [jmApp1..jmAppSpecificLast, jmComment]
        else if S = 'exif' then
          Include(StripKinds, mkExif)
        else if S = 'iptc' then
          Include(StripKinds, mkIPTC)
        else if S = 'xmp' then
          Include(StripKinds, mkXMP)
        else if S = 'com' then
          Include(StripSegments, jmComment)
        else if S = 'backup' then
          MakeBackup := True
        else if S = 'quiet' then
          Quiet := True
        else if (Copy(S, 1, 3) = 'app') and TryStrToInt(Copy(S, 4, MaxInt), RetVal) and
          (RetVal >= 0) and (RetVal <= 15) then
          Include(StripSegments, jmAppSpecificFirst + RetVal)
        else
        begin
          if (S = '?') or (S = 'help') then
            OutputInfo
          else
            Writeln(Format('Invalid switch (%s)', [S]));
          Exit;
        end
      end;
    end;
    if Files.Count = 0 then
    begin
      OutputInfo;
      Exit;
    end;
    if (StripKinds = []) and (StripSegments = []) then
      StripSegments := [jmApp1..jmAppSpecificLast, jmComment]
    else
    begin
      if jmApp13 in StripSegments then Exclude(StripKinds, mkIPTC);
      if jmApp1 in StripSegments then StripKinds := StripKinds - [mkExif, mkXMP];
    end;
    for I := 0 to Files.Count - 1 do
    begin
      S := Files[I];
      RetVal := FindFirst(S, faReadOnly, SearchRec);
      if RetVal <> 0 then
      begin
        Writeln('Invalid source file name (', S, '):');
        Writeln(SysErrorMessage(RetVal));
        Continue;
      end;
      S := ExtractFilePath(S);
      repeat
        try
          ProcessFile(S + SearchRec.Name);
        except
          on E: Exception do
            OutputProcessingError(S + SearchRec.Name, E.Message)
          else
            raise;
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    Files.Free;
  end;
end;

begin
  try
    if ParamCount = 0 then
      MessageBox(0, 'This is a console application that requires command-line ' +
        'arguments - use StripJPEGMetadata /? for help on what they should be.',
        'JPEG Metadata Stripper v' + CCRExifVersion, MB_ICONINFORMATION)
    else
      DoIt;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  if DebugHook <> 0 then Readln;
end.
