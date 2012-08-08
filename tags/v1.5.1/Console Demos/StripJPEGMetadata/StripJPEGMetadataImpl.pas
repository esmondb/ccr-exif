{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
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
{ The Original Code is StripJPEGMetadataImpl.pas.                                      }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit StripJPEGMetadataImpl;
{
  The code here would normally be in the DPR. It is in a separate PAS so that we can
  have separate DPROJ files for different Delphi versions - unfortunately, there is a
  one-to-one correspondence between a DPR (for code) and a DPROJ (for version-specific
  settings).
}
interface

{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF MSWINDOWS}
{$R WindowsXP.res}
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ELSE}IOUtils,{$ENDIF} Types, SysUtils, Classes, StrUtils,
  CCR.Exif, CCR.Exif.BaseUtils, CCR.Exif.Consts;

procedure OutputInfo;
var
  Stream: TResourceStream;
  Text: string;
begin
  Stream := TResourceStream.Create(HInstance, 'HelpText', RT_RCDATA);
  try
    Text := UTF8ToString(Stream.Memory, Stream.Size)
  finally
    Stream.Free;
  end;
  WriteLn(Format(Text, [ChangeFileExt(ExtractFileName(GetExecutableName), ''),
    CCRExifVersion]));
end;

procedure OutputProcessingError(const JpegFile, Msg: string);
begin
  Writeln('Problem processing ', JpegFile, ':');
  Writeln(Msg);
  Writeln('');
end;

function FileCopy(const ASourceFile, ADestFile: string; AFailIfExists: Boolean): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := CopyFile(PChar(ASourceFile), PChar(ADestFile), AFailIfExists);
{$ELSE}
  try
    TFile.Copy(ASourceFile, ADestFile, not AFailIfExists);
    Result := True;
  except
    on EFileNotFoundException do Result := False;
    on EInOutError do Result := False;
    on EArgumentException do Result := False;
  end;
{$ENDIF}
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
    if not FileExists(BackupFN) and not FileCopy(JpegFile, BackupFN, False) then
    begin
      OutputProcessingError(JpegFile, SysErrorMessage(GetLastError));
      Exit;
    end;
  end;
  RemovedKinds := RemoveMetadataFromJPEG(JpegFile, StripKinds);   //defined and implemented in CCR.Exif.pas
  RemovedSegments := RemoveJPEGSegments(JpegFile, StripSegments); //defined and implemented in CCR.Exif.BaseUtils.pas
  if not Quiet then
    if (RemovedKinds <> []) or (RemovedSegments <> []) then
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
    {$IFDEF MSWINDOWS}
    if ParamCount = 0 then
    begin
      MessageBox(0, 'This is a console application that requires command-line ' +
        'arguments - use StripJPEGMetadata /? for further information.',
        'JPEG Metadata Stripper v' + CCRExifVersion, MB_ICONINFORMATION);
      Exit;
    end;
    {$ENDIF}
    DoIt;
  except
    on E: Exception do WriteLn(E.ClassName, ': ', E.Message);
  end;
  {$IFDEF MSWINDOWS}
  if DebugHook <> 0 then Readln;
  {$ENDIF}
end.
