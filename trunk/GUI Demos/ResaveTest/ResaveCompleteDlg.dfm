object frmOpenFile: TfrmOpenFile
  Left = 246
  Top = 92
  BorderStyle = bsDialog
  Caption = 'Resave Completed'
  ClientHeight = 256
  ClientWidth = 277
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    AlignWithMargins = True
    Left = 103
    Top = 223
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
  end
  object grpNewFile: TGroupBox
    Left = 8
    Top = 129
    Width = 257
    Height = 85
    Caption = ' New file.jpg '
    TabOrder = 1
    object btnOpenInDefaultApp: TButton
      AlignWithMargins = True
      Left = 8
      Top = 18
      Width = 241
      Height = 25
      Caption = 'Open in the &default program for JPEG files'
      TabOrder = 0
      OnClick = btnOpenInDefaultAppClick
    end
    object btnOpenInExplorer: TButton
      AlignWithMargins = True
      Left = 8
      Top = 49
      Width = 161
      Height = 25
      Caption = 'Open in &Windows Explorer'
      TabOrder = 1
      OnClick = btnOpenInExplorerClick
    end
    object btnDeleteFile: TButton
      AlignWithMargins = True
      Left = 175
      Top = 49
      Width = 74
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteFileClick
    end
  end
  object grpCompare: TGroupBox
    Left = 8
    Top = 8
    Width = 257
    Height = 116
    Caption = ' Compare '
    TabOrder = 0
    object btnCompareInExifList: TButton
      Left = 8
      Top = 18
      Width = 241
      Height = 25
      Caption = 'Compare in &Exif Tag Lister'
      TabOrder = 0
      OnClick = btnOpenInDemoClick
    end
    object btnCompareInJpegDump: TButton
      Left = 8
      Top = 49
      Width = 241
      Height = 25
      Caption = 'Compare in &JPEG Header Dump'
      TabOrder = 1
      OnClick = btnOpenInDemoClick
    end
    object btnCompareInXMPBrowser: TButton
      Left = 8
      Top = 80
      Width = 241
      Height = 25
      Caption = 'Compare in &XMP Browser'
      TabOrder = 2
      OnClick = btnOpenInDemoClick
    end
  end
end
