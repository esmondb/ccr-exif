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
{ The Original Code is CreateXMPSidecarImpl.pas.                                       }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit CreateXMPSidecarImpl;
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
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} Types, SysUtils, Classes, WideStrUtils,
  CCR.Exif, CCR.Exif.BaseUtils, CCR.Exif.Consts, CCR.Exif.XMPUtils;

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

procedure DoIt(const JpegFile: string);
var
  ExifData: TExifData;
  DestFile: string;
  XML: UTF8String;
  OutputStream: TFileStream;
begin
  OutputStream := nil;
  ExifData := TExifData.Create;
  try
    //create XMP data from the existing Exif tags
    ExifData.LoadFromGraphic(JpegFile);
    ExifData.XMPWritePolicy := xwAlwaysUpdate;
    ExifData.Rewrite;
    //determine the output file name
    DestFile := ParamStr(2);
    if DestFile = '' then
      DestFile := ChangeFileExt(JpegFile, '.xmp')
    else if DestFile[1] = '*' then
      DestFile := ChangeFileExt(JpegFile, '') + Copy(DestFile, 2, MaxInt);
    OutputStream := TFileStream.Create(DestFile, fmCreate);
    //write a BOM if a text file extension is being used
    if SameText(ExtractFileExt(DestFile), '.txt') then
      OutputStream.WriteBuffer(sUTF8BOMString, SizeOf(sUTF8BOMString));
    //write the XML
    XML := ExifData.XMPPacket.RawXML;
    OutputStream.WriteBuffer(XML[1], Length(XML));
  finally
    ExifData.Free;
    OutputStream.Free;
  end;
  Writeln('Created ', DestFile);
end;

var
  RetVal: Integer;
  FileSpec, Path: string;
  SearchRec: TSearchRec;
begin
  try
    {$IFDEF MSWINDOWS}
    if ParamCount = 0 then
    begin
      MessageBox(0, 'This is a console application that requires command-line ' +
        'arguments - use CreateXMPSidecar /? for further information.',
        'XMP Sidecar Creator v' + CCRExifVersion, MB_ICONINFORMATION);
      Exit;
    end;
    {$ENDIF}
    FileSpec := ParamStr(1);
    if (FileSpec = '') or (AnsiChar(FileSpec[1]) in SwitchChars + ['?']) then
    begin
      OutputInfo;
      Exit;
    end;
    Path := ExtractFilePath(FileSpec);
    if Path = '' then
    begin
      Path := IncludeTrailingPathDelimiter(GetCurrentDir);
      FileSpec := Path + FileSpec;
    end;
    RetVal := FindFirst(FileSpec, faReadOnly or faArchive, SearchRec);
    if RetVal <> 0 then
    begin
      WriteLn('No file matched to "' + ParamStr(1) + '"');
      Exit;
    end;
    repeat
      try
        DoIt(Path + SearchRec.Name);
      except
        on E:Exception do
        begin
          Writeln('Problem processing ', Path + SearchRec.Name, ' (', E.Classname, '):');
          Writeln(E.Message);
          Writeln('');
        end
        else
          raise;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  except
    on E: Exception do WriteLn(E.ClassName, ': ', E.Message);
  end;
  {$IFDEF MSWINDOWS}
  if DebugHook <> 0 then ReadLn;
  {$ENDIF}
end.
