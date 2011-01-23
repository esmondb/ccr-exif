object frmXMPBrowser: TfrmXMPBrowser
  Left = 433
  Top = 235
  BorderWidth = 4
  Caption = 'XMP Browser'
  ClientHeight = 439
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object panFooter: TPanel
    Left = 0
    Top = 403
    Width = 583
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      583
      36)
    object lblURI: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 374
      Height = 30
      Hint = 'URI'
      Align = alLeft
      AutoSize = False
      EllipsisPosition = epWordEllipsis
      PopupMenu = mnuURI
      ShowAccelChar = False
      Layout = tlCenter
      OnContextPopup = lblURIContextPopup
      ExplicitLeft = 31
      ExplicitTop = 0
      ExplicitHeight = 36
    end
    object btnOpen: TBitBtn
      Left = 411
      Top = 6
      Width = 87
      Height = 27
      Action = actOpen
      Anchors = [akTop, akRight]
      Caption = '&Open File'
      Default = True
      TabOrder = 0
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
        078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
        BE078DBEFF00FFFF00FF078DBE25A1D172C7E785D7FA66CDF965CDF965CDF965
        CDF965CDF865CDF965CDF866CEF939ADD8078DBEFF00FFFF00FF078DBE4CBCE7
        39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
        D984D7EB078DBEFF00FF078DBE72D6FA078DBEAEEAFC79DCFB79DCFB79DCFB79
        DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9AEF1F9078DBEFF00FF078DBE79DDFB
        1899C79ADFF392E7FB84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
        DAB3F4F9078DBEFF00FF078DBE82E3FC43B7DC65C3E0ACF0FD8DEBFC8DEBFC8D
        EBFD8DEBFD8DEBFC8DEBFD0C85184CBBDAB6F7F96DCAE0078DBE078DBE8AEAFC
        77DCF3229CC6FDFFFFC8F7FEC9F7FEC9F7FEC9F7FEC8F7FE0C85183CBC5D0C85
        18DEF9FBD6F6F9078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
        8DBE078DBE0C851852D97F62ED9741C4650C8518078DBE078DBE078DBE9BF5FE
        9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE0C851846CE6C59E48858E18861EB
        9440C1650C8518FF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFE0C
        85180C85180C85180C851856E18447CD6E0C85180C85180C8518FF00FF078DBE
        FEFEFEA5FEFFA5FEFFA5FEFF078CB643B7DC43B7DC43B7DC0C85184EDD7936BA
        540C8518FF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
        00FFFF00FFFF00FF0C851840D0650C8518FF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85182AB7432DBA490C85
        18FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FF0C851821B5380C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C85180C8518FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C
        85180C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    end
    object btnExit: TBitBtn
      Left = 504
      Top = 6
      Width = 75
      Height = 27
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Exit'
      TabOrder = 1
      OnClick = btnExitClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 583
    Height = 403
    ActivePage = tabOriginal
    Align = alClient
    TabOrder = 1
    object tabOriginal: TTabSheet
      Caption = 'Original'
    end
    object tabResaved: TTabSheet
      Caption = 'Resaved'
      ImageIndex = 1
    end
  end
  object dlgOpen: TOpenPictureDialog
    DefaultExt = 'jpg'
    Filter = 
      'Supported image and sidecars files|*.jpg;*.jpeg;*.psd;*.tif;*.ti' +
      'ff;*.xmp|JPEG images (*.jpg, *.jpeg)|*.jpg;*.jpeg|Photoshop imag' +
      'es (*.psd)|*.psd|TIFF images (*.tif, *.tiff)|*.tif;*.tiff|XMP si' +
      'decar files (*.xmp)|*.xmp|XML files (*.xml)|*.xml|Text files (*.' +
      'txt)|*.txt'
    Options = [ofHideReadOnly, ofExtensionDifferent, ofFileMustExist, ofEnableSizing]
    Left = 104
    Top = 352
  end
  object ActionList: TActionList
    Left = 140
    Top = 352
    object actOpen: TAction
      Caption = '&Open File'
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
  end
  object mnuURI: TPopupMenu
    Left = 200
    Top = 352
    object itmCopyURI: TMenuItem
      Caption = 'Copy URI'
      OnClick = itmCopyURIClick
    end
  end
end
