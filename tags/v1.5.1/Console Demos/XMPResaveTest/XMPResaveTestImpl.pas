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
{ The Original Code is XMPResaveTestImpl.pas.                                          }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit XMPResaveTestImpl;
{
  Tests XMP roundtripping in light of TXMPPacket now doing its own saving. Note that the
  resaved data may be larger than the original if the latter used XML attributes rather
  than (only) element nodes to store XMP properties. On the other hand, my code doesn't
  write out any padding at the end of the packet, and produces neat, human-readable XML.
}
interface

{$DEFINE OUTPUTXML}
{.$DEFINE FORCEADOM}

implementation

uses
  {$IFDEF FORCEADOM}XmlDom,{$IF CompilerVersion >=21}AdomXmlDom{$ELSE}OXmlDom{$IFEND},{$ENDIF}
  SysUtils, Classes,
  CCR.Exif.BaseUtils, CCR.Exif.Consts, CCR.Exif.StreamHelper, CCR.Exif.XMPUtils;

var
  DelphiVersion: string;

procedure Test(const AFileName: string);
var
  Packet1, Packet2: TXMPPacket;
  I, J, K: Integer;
  Prop1, Prop2, SubProp1, SubProp2: TXMPProperty;
begin
  WriteLn;
  Writeln(AFileName);
  Writeln;
  Packet2 := nil;
  Packet1 := TXMPPacket.Create;
  try
    if Packet1.LoadFromGraphic(AFileName) then
    begin
      if Packet1.Empty then
      begin
        Writeln('Cannot test, as source file contains no XMP data');
        Exit;
      end;
    end
    else
      Packet1.LoadFromFile(AFileName);
    Writeln('Original XML totals ', Length(Packet1.RawXML), ' bytes');
    Packet1.ResetRawXMLCache; //force a rewrite of the XML
    Writeln('Rewritten XML totals ', Length(Packet1.RawXML), ' bytes');
    {$IFDEF OUTPUTXML}
    with TFileStream.Create(ChangeFileExt(AFileName, Format('.%s', [DelphiVersion])), fmCreate) do
    try
      WriteUTF8Chars(Packet1.RawXML);
    finally
      Free;
    end;
    {$ENDIF}
    Writeln;
    Packet2 := TXMPPacket.Create;
    Packet2.RawXML := Packet1.RawXML;
    if Packet1.SchemaCount <> Packet2.SchemaCount then
    begin
      Writeln('FAILED: SchemaCount different');
      Exit;
    end;
    for I := 0 to Packet1.SchemaCount - 1 do
    begin
      if Packet1[I].NamespaceInfo.URI <> Packet2[I].NamespaceInfo.URI then
      begin
        Writeln('FAILED: Schemas[', I, '].URI different');
        Exit;
      end;
      if Packet1[I].PropertyCount <> Packet2[I].PropertyCount then
      begin
        Writeln('FAILED: Schemas[', I, '].PropertyCount different');
        Exit;
      end;
      for J := 0 to Packet1[I].PropertyCount - 1 do
      begin
        Prop1 := Packet1[I][J];
        Prop2 := Packet2[I][J];
        if Prop1.Kind <> Prop2.Kind then
        begin
          Writeln('FAILED: Schemas[', I, '].Properties[', J, '].Kind different');
          Exit;
        end;
        if Prop1.Name <> Prop2.Name then
        begin
          Writeln('FAILED: Schemas[', I, '].Properties[', J, '].Name different');
          Exit;
        end;
        if Prop1.ReadValue <> Prop2.ReadValue then
        begin
          Writeln('FAILED: Schemas[', I, '].Properties[', J, '].ReadValue different');
          Exit;
        end;
        if Prop1.SubPropertyCount <> Prop2.SubPropertyCount then
        begin
          Writeln('FAILED: Schemas[', I, '].Properties[', J, '].SubPropertyCount different');
          Exit;
        end;
        for K := 0 to Prop1.SubPropertyCount - 1 do
        begin
          SubProp1 := Prop1[K];
          SubProp2 := Prop2[K];
          if SubProp1.Kind <> SubProp2.Kind then
          begin
            Writeln('FAILED: Schemas[', I, '].Properties[', J, '].SubProperties[', K, '].Kind different');
            Exit;
          end;
          if SubProp1.Name <> SubProp2.Name then
          begin
            Writeln('FAILED: Schemas[', I, '].Properties[', J, '].SubProperties[', K, '].Name different');
            Exit;
          end;
          if SubProp1.ReadValue <> SubProp2.ReadValue then
          begin
            Writeln('FAILED: Schemas[', I, '].Properties[', J, '].SubProperties[', K, '].ReadValue different');
            Exit;
          end;
          if SubProp1.SubPropertyCount <> SubProp2.SubPropertyCount then
          begin
            Writeln('FAILED: Schemas[', I, '].Properties[', J, '].SubProperties[', K, '].SubPropertyCount different');
            Exit;
          end;
        end;
      end;
    end;
  finally
    Packet1.Free;
    Packet2.Free;
  end;
  Writeln('Rountrip was a success!');
end;

var
  I: Integer;
begin
  try
    if CompilerVersion = 18.0 then
      DelphiVersion := '2006'
    else if CompilerVersion = 18.5 then
      DelphiVersion := '2007'
    else if CompilerVersion = 19.0 then
      DelphiVersion := '2007.NET' //which would be a surprise, given my code doesn't support DCCIL!
    else if CompilerVersion = 20.0 then
      DelphiVersion := '2009'
    else if CompilerVersion = 21.0 then
      DelphiVersion := '2010'
    else if CompilerVersion = 22.0 then
      DelphiVersion := 'XE'
    else if CompilerVersion = 23.0 then
      DelphiVersion := 'XE2'
    else
      DelphiVersion := 'NotDeadYet';
    Writeln('');
    if ParamCount = 0 then
      Writeln('Tests TXMPPacket''s saving code by roundtripping XMP data in memory.' + sLineBreak +
        'To use, pass one or more JPEG or XMP files on the command line.')
    else
    begin
      Writeln('CCR Exif version: ', CCRExifVersion);
      Write('Compiler version: Delphi ', DelphiVersion);
      {$IFDEF FORCEADOM}
        {$IF CompilerVersion >=21}
        DefaultDOMVendor := OpenXML4Factory.Description;
        {$ELSE}
        DefaultDOMVendor := OpenXMLFactory.Description;
        {$IFEND}
      {$ENDIF}
      for I := 1 to ParamCount do
        Test(ParamStr(I));
    end;
  except
    on E: Exception do WriteLn(E.ClassName, ': ', E.Message);
  end;
  {$IFDEF MSWINDOWS}
  WriteLn;
  Write('Press ENTER to exit');
  ReadLn;
  {$ENDIF}
end.
