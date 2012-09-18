object dtmController: TdtmController
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object MacMenu: TMainMenu
    Left = 151
    Top = 54
    object itmApp: TMenuItem
      Height = 50.000000000000000000
      Text = 'XMP Browser'
      Width = 50.000000000000000000
      object MenuItem5: TMenuItem
        Action = actAppAbout
        Height = 50.000000000000000000
        Locked = True
        Text = 'About XMP Browser'
        Width = 50.000000000000000000
      end
      object itmDiv: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object itmHideMe: TMenuItem
        Action = actAppHide
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmHideOtherApps: TMenuItem
        Action = actAppHideOthers
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmShowAll: TMenuItem
        Action = actAppShowAll
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem6: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object itmAppQuit: TMenuItem
        Action = actAppQuit
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
    object itmFile: TMenuItem
      Height = 50.000000000000000000
      Text = '&File'
      Width = 50.000000000000000000
      object itmFileOpen: TMenuItem
        Action = actOpenFile
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object itmFileClose: TMenuItem
        Action = actClose
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
    object MenuItem8: TMenuItem
      Height = 50.000000000000000000
      Text = 'View'
      Width = 50.000000000000000000
      object MenuItem9: TMenuItem
        Action = actToggleFullScreen
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
    object itmWindow: TMenuItem
      Height = 50.000000000000000000
      Text = 'Window'
      Width = 50.000000000000000000
      object itmMinimize: TMenuItem
        Action = actMinimizeWindow
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem1: TMenuItem
        Action = actZoomWindow
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
      object MenuItem2: TMenuItem
        Height = 50.000000000000000000
        Locked = True
        Text = '-'
        Width = 50.000000000000000000
      end
      object itmBringAllToFront: TMenuItem
        Action = actBringAllToFront
        Height = 50.000000000000000000
        Locked = True
        Width = 50.000000000000000000
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'jpg'
    Filter = 
      'All supported image types|*.jpg;*.jpeg;*.tif;*.tiff|JPEG images ' +
      '(*.jpg,*.jpeg)|*.jpg;*.jpeg|TIFF images (*.tif,*.tiff)|*.tif;*.t' +
      'iff'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 87
    Top = 54
  end
  object ActionList: TActionList
    Left = 24
    Top = 54
    object actOpenFile: TAction
      Category = 'File'
      Text = '&Open...'
      ShortCut = 4175
      OnExecute = actOpenFileExecute
    end
    object actClose: TWindowClose
      Category = 'File'
      CustomText = 'Close'
    end
    object actAppQuit: TFileExit
      Category = 'Application'
    end
    object actAppHide: TFileHideApp
      Category = 'Application'
    end
    object actAppHideOthers: TFileHideAppOthers
      Category = 'Application'
    end
    object actNextTab: TAction
      Category = 'Tab'
      Text = 'actNextTab'
      ShortCut = 16393
      SecondaryShortCuts.Strings = (
        'Shift+Cmd+]')
    end
    object actPreviousTab: TAction
      Category = 'Tab'
      Text = 'actPreviousTab'
      ShortCut = 24585
      SecondaryShortCuts.Strings = (
        'Shift+Cmd+[')
    end
    object actMinimizeWindow: TAction
      Category = 'Window'
      Text = 'Minimize'
      ShortCut = 4173
    end
    object actZoomWindow: TAction
      Category = 'Window'
      Text = 'Zoom'
    end
    object actAppShowAll: TAction
      Category = 'Application'
      Text = 'Show All'
    end
    object actBringAllToFront: TAction
      Category = 'Window'
      Text = 'Bring All to Front'
    end
    object actAppAbout: TAction
      Category = 'Application'
      Text = 'About Exif List'
    end
    object actToggleFullScreen: TAction
      Category = 'Window'
      Text = 'Enter Full Screen'
      ShortCut = 20550
    end
  end
end
