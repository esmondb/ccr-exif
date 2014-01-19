object frmScreenshotDemo: TfrmScreenshotDemo
  Left = 246
  Top = 92
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Exif Creation Demo'
  ClientHeight = 238
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblAuthor: TLabel
    Left = 8
    Top = 11
    Width = 37
    Height = 13
    Caption = '&Author:'
    FocusControl = edtAuthor
  end
  object lblComments: TLabel
    Left = 8
    Top = 93
    Width = 54
    Height = 13
    Caption = '&Comments:'
    FocusControl = edtComments
  end
  object lblTags: TLabel
    Left = 8
    Top = 120
    Width = 27
    Height = 13
    Caption = 'Tag&s:'
    FocusControl = edtKeywords
  end
  object lblRating: TLabel
    Left = 8
    Top = 147
    Width = 35
    Height = 13
    Caption = '&Rating:'
    FocusControl = cboRating
  end
  object lblTitle: TLabel
    Left = 8
    Top = 39
    Width = 24
    Height = 13
    Caption = '&Title:'
    FocusControl = edtTitle
  end
  object Label6: TLabel
    Left = 8
    Top = 66
    Width = 40
    Height = 13
    Caption = '&Subject:'
    FocusControl = edtSubject
  end
  object edtAuthor: TEdit
    Left = 76
    Top = 8
    Width = 212
    Height = 21
    TabOrder = 0
  end
  object edtComments: TEdit
    Left = 76
    Top = 90
    Width = 212
    Height = 21
    TabOrder = 3
    Text = 'Created with CCR Exif v'
  end
  object edtKeywords: TEdit
    Left = 76
    Top = 117
    Width = 212
    Height = 21
    TabOrder = 4
    Text = 'screenshot,exif,demo'
  end
  object chkThumbnail: TCheckBox
    Left = 8
    Top = 177
    Width = 113
    Height = 17
    Caption = '&Include thumbnail'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object btnCreate: TButton
    Left = 8
    Top = 204
    Width = 199
    Height = 25
    Caption = 'Create Screenshot'
    Default = True
    TabOrder = 8
    OnClick = btnCreateClick
  end
  object btnClose: TButton
    Left = 213
    Top = 204
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 9
    OnClick = btnCloseClick
  end
  object cboRating: TComboBox
    Left = 76
    Top = 144
    Width = 212
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = '<undefined>'
    Items.Strings = (
      '<undefined>'
      '1 star'
      '2 stars'
      '3 stars'
      '4 stars'
      '5 stars')
  end
  object edtTitle: TEdit
    Left = 76
    Top = 36
    Width = 212
    Height = 21
    TabOrder = 1
    Text = 'Test Screenshot'
  end
  object edtSubject: TEdit
    Left = 76
    Top = 63
    Width = 212
    Height = 21
    TabOrder = 2
    Text = 'Demos'
  end
  object chkWriteXMP: TCheckBox
    Left = 139
    Top = 177
    Width = 149
    Height = 17
    Caption = 'Write &XMP metadata too'
    TabOrder = 7
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'jpg'
    Filter = 'JPEG Image File (*.jpg,*.jpeg)|*.jpg;*.jpeg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 252
    Top = 7
  end
end
