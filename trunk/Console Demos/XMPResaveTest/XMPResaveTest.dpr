program XMPResaveTest;
{
  Tests XMP roundtripping in light of TXMPPacket now doing its own saving. Note that the
  resaved data may be larger than the original if the latter used XML attributes rather
  than (only) element nodes to store XMP properties. On the other hand, my code doesn't
  write out any padding at the end of the packet, and produces neat, human-readable XML.
}
{$APPTYPE CONSOLE}
{$DEFINE OUTPUTXML}

uses
  SysUtils,
  Classes,
  //CCR.UseMSXML6 in 'CCR.UseMSXML6.pas',
  CCR.Exif.BaseUtils in '..\..\CCR.Exif.BaseUtils.pas',
  CCR.Exif.Consts in '..\..\CCR.Exif.Consts.pas',
  CCR.Exif.StreamHelper in '..\..\CCR.Exif.StreamHelper.pas',
  CCR.Exif.TagIDs in '..\..\CCR.Exif.TagIDs.pas',
  CCR.Exif.TiffUtils in '..\..\CCR.Exif.TiffUtils.pas',
  CCR.Exif.XMPUtils in '..\..\CCR.Exif.XMPUtils.pas';

type
  TXMPPacket = class(CCR.Exif.XMPUtils.TXMPPacket); //get access to protected members

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
    Packet1.Changed; //invalidates internal cache of the packet's XML
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
    else
      DelphiVersion := 'NotDeadYet';
    if ParamCount = 0 then
      Writeln('Tests TXMPPacket''s saving code by roundtripping XMP data in memory. ' +
        'To use, pass one or more JPEG or XMP files on the command line.')
    else
    begin
      Writeln('CCR Exif version: ', CCRExifVersion);
      Write('Compiler version: Delphi ', DelphiVersion);
      for I := 1 to ParamCount do
        Test(ParamStr(I));
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  WriteLn;
  Write('Press ENTER to exit');
  ReadLn;
end.
