unit CCR.UseMSXML6;

interface

implementation

{$IF CompilerVersion < 20.0}
uses ActiveX, MSXML, MSXMLDOM;

function CreateDOMDocumentEx: IXMLDOMDocument;
const
  CLASS_DOMDocument60: TGUID = '{88D96A05-F192-11D4-A65F-0040963251E5}';
begin
  Result := nil;
  if CoCreateInstance(CLASS_DOMDocument60, nil, CLSCTX_INPROC_SERVER or
      CLSCTX_LOCAL_SERVER, IXMLDOMDocument, Result) <> S_OK then
    Result := CreateDOMDocument;
end;

initialization
  MSXMLDOMDocumentCreate := CreateDOMDocumentEx;
{$IFEND}

end.
