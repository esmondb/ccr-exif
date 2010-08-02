{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.1 (2010-08-02)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CreateXMPSidecar.dpr.                                           }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

program CreateXMPSidecar;

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows,
  SysUtils,
  Classes,
  WideStrUtils,
  XPMan,
  CCR.Exif in '..\..\CCR.Exif.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.IPTC in '..\..\CCR.Exif.IPTC.pas',
  CCR.Exif.JPEGUtils in '..\..\CCR.Exif.JPEGUtils.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas';

{$R *.res}

procedure OutputInfo;
begin
  Writeln('----------------------------------------------------');
  Writeln('             XMP Sidecar Creator v' + CCRExifVersion);
  Writeln('----------------------------------------------------');
  Writeln('');
  Writeln('Creates an XMP sidecar file for a given JPEG image,');
  Writeln('using data stored in the latter''s Exif segment. If');
  Writeln('the JPEG contains an XMP packet too, properties from');
  Writeln('the latter are outputted as well.');
  Writeln('');
  Writeln('Usage:');
  Writeln('CreateXMPSidecar SourceJpegFile [OutputXMPFile]');
  Writeln('');
  WriteLn('Notes:');
  Writeln('- If OutputXMPFile is not specified, the source file' + sLineBreak +
          '  name with its extension changed to .xmp is used.' + sLineBreak +
          '- Both SourceJpegFile and OutputXMPFile can include' + sLineBreak +
          '  an asterisk wildcard.');
  Writeln('');
  Writeln('Examples:');
  Writeln('CreateXMPSidecar "My Photo.jpg"');
  Writeln('CreateXMPSidecar "My Photo.jpg" *.xml');
  Writeln('CreateXMPSidecar "My Photo.jpg" "My Photo (tags).txt"');
  Writeln('CreateXMPSidecar *.jpg *.xml');
  Writeln('CreateXMPSidecar *.jpg "* (tags).txt"');
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
    ExifData.LoadFromJPEG(JpegFile);
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
  S: string;
  SearchRec: TSearchRec;
begin
  try
    if ParamCount = 0 then
    begin
      MessageBox(0, 'This is a console application that requires command-line arguments' +
        ' - use CreateXMPSidecar /? for help on what they should be.',
        'XMP Sidecar Creator v' + CCRExifVersion, MB_ICONINFORMATION);
      Exit;
    end;
    S := ParamStr(1);
    if (S = '') or (AnsiChar(S[1]) in SwitchChars + ['?']) then
    begin
      OutputInfo;
      Exit;
    end;
    RetVal := FindFirst(S, faReadOnly or faArchive, SearchRec);
    if RetVal <> 0 then
    begin
      Writeln('Invalid parameter (', S, '):');
      Writeln(SysErrorMessage(RetVal));
      Exit;
    end;
    S := ExtractFilePath(S);
    repeat
      try
        DoIt(S + SearchRec.Name);
      except
        on E:Exception do
        begin
          Writeln('Problem processing ', S + SearchRec.Name, ' (', E.Classname, '):');
          Writeln(E.Message);
          Writeln('');
        end
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
