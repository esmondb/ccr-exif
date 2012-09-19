{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.2                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ExifListUtils;

interface

uses
  System.SysUtils, CCR.Exif;

const
  SGeneral = 'General';
  SEndiannessCaption = 'Byte order';
  SEndianness: array[TEndianness] of string = ('Small endian', 'Big endian');
  SNoYes: array[Boolean] of string = ('No', 'Yes');
  SUnrecognized = '[Unrecognised value (%d)]';

function ColorSpaceToStr(Value: TExifColorSpace): string;
function DirectionRefToStr(Value: TGPSDirectionRef): string;
function DistanceRefToStr(Value: TGPSDistanceRef): string;
function ExposureProgramToStr(Value: TExifExposureProgram): string;
function FileSourceToStr(Value: TExifFileSource): string;
function FlashModeToStr(Value: TExifFlashMode): string;
function GainControlToStr(Value: TExifGainControl): string;
function GPSAltitudeRefToStr(Value: TGPSAltitudeRef): string;
function GPSDifferentialToStr(Value: TGPSDifferential): string;
function GPSMeasureModeToStr(Value: TGPSMeasureMode): string;
function GPSSpeedRefToStr(Value: TGPSSpeedRef): string;
function GPSStatusToStr(Value: TGPSStatus): string;
function LightSourceToStr(Value: TExifLightSource): string;
function MeteringModeToStr(Value: TExifMeteringMode): string;
function OrientationToStr(Value: TTiffOrientation): string;
function RenderingToStr(Value: TExifRendering): string;
function ResolutionUnitsToStr(Value: TTiffResolutionUnit): string;
function SceneCaptureTypeToStr(Value: TExifSceneCaptureType): string;
function SceneTypeToStr(Value: TExifSceneType): string;
function SensingMethodToStr(Value: TExifSensingMethod): string;
function StrobeLightToStr(Value: TExifStrobeLight): string;
function SubjectDistanceRangeToStr(Value: TExifSubjectDistanceRange): string;
function WhiteBalanceModeToStr(Value: TExifWhiteBalanceMode): string;
function DateTimeToStr(const Value: TDateTime): string;

implementation

function ColorSpaceToStr(Value: TExifColorSpace): string;
begin
  case Value of
    csTagMissing: Result := '';
    csRGB: Result := 'sRGB';
    csAdobeRGB: Result := 'Adobe RGB';
    csWideGamutRGB: Result := 'Wide gamut RGB (Sony)';
    csICCProfile: Result := 'ICC profile (Sony)';
    csUncalibrated: Result := 'Uncalibrated (maybe Adobe RGB)';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function DirectionRefToStr(Value: TGPSDirectionRef): string;
begin
  case Value of
    drMissingOrInvalid, drTrueNorth: Result := 'true north';
    drMagneticNorth: Result := 'magnetic north';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function DistanceRefToStr(Value: TGPSDistanceRef): string;
begin
  case Value of //KM is the default according to the Exif spec
    dsMissingOrInvalid, dsKilometres: Result := 'kilometres';
    dsMiles: Result := 'miles';
    dsKnots: Result := 'knots';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function ExposureProgramToStr(Value: TExifExposureProgram): string;
begin
  case Value of
    eeTagMissing: Result := '';
    eeUndefined: Result := 'Undefined';
    eeManual: Result := 'Manual';
    eeNormal: Result := 'Normal';
    eeAperturePriority: Result := 'Aperture priority';
    eeShutterPriority: Result := 'Shutter priority';
    eeCreative: Result := 'Creative';
    eeAction: Result := 'Action';
    eePortrait: Result := 'Portrait';
    eeLandscape: Result := 'Landscape';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function FileSourceToStr(Value: TExifFileSource): string;
begin
  case Value of
    fsUnknown: Result := '';
    fsFilmScanner: Result := 'Film scanner';
    fsReflectionPrintScanner: Result := 'Reflection print scanner';
    fsDigitalCamera: Result := 'Digital camera';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function FlashModeToStr(Value: TExifFlashMode): string;
begin
  case Value of
    efUnknown: Result := '';
    efCompulsoryFire: Result := 'Compulsory fire';
    efCompulsorySuppression: Result := 'Compulsory suppression';
    efAuto: Result := 'Auto';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GainControlToStr(Value: TExifGainControl): string;
begin
  case Value of
    egTagMissing: Result := '';
    egNone: Result := 'None';
    egLowGainUp: Result := 'Low gain up';
    egHighGainUp: Result := 'High gain up';
    egLowGainDown: Result := 'Low gain down';
    egHighGainDown: Result := 'High gain down';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSAltitudeRefToStr(Value: TGPSAltitudeRef): string;
begin
  case Value of
    alTagMissing: Result := '';
    alAboveSeaLevel: Result := 'above sea level';
    alBelowSeaLevel: Result := 'below sea level';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSDifferentialToStr(Value: TGPSDifferential): string;
begin
  case Value of
    dfTagMissing: Result := '';
    dfWithoutCorrection: Result := 'Without correction';
    dfCorrectionApplied: Result := 'Correction applied';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSMeasureModeToStr(Value: TGPSMeasureMode): string;
begin
  case Value of
    mmUnknown: Result := '';
    mm2D: Result := '2D';
    mm3D: Result := '3D';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSSpeedRefToStr(Value: TGPSSpeedRef): string;
begin
  case Value of
    srMissingOrInvalid, srKilometresPerHour: Result := 'km/h'; //km/h is the default according to the Exit spec
    srMilesPerHour: Result := 'mph';
    srKnots: Result := 'knots';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function GPSStatusToStr(Value: TGPSStatus): string;
begin
  case Value of
    stMissingOrInvalid: Result := '';
    stMeasurementActive: Result := 'Measurement active/in progress';
    stMeasurementVoid: Result := 'Measurement void/in interop';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function LightSourceToStr(Value: TExifLightSource): string;
begin
  case Value of
    elTagMissing: Result := '';
    elUnknown: Result := 'Unknown';
    elDaylight: Result := 'Daylight';
    elFluorescent: Result := 'Fluorescent';
    elTungsten: Result := 'Tungsten';
    elFlash: Result := 'Flash';
    elFineWeather: Result := 'Fine weather';
    elCloudyWeather: Result := 'Cloudy weather';
    elShade: Result := 'Shade';
    elDaylightFluorescent: Result := 'Daylight fluorescent';
    elDayWhiteFluorescent: Result := 'DayWhite fluorescent';
    elCoolWhiteFluorescent: Result := 'Cool white fluorescent';
    elWhiteFluorescent: Result := 'White fluorescent';
    elStandardLightA: Result := 'Standard light A';
    elStandardLightB: Result := 'Standard light B';
    elStandardLightC: Result := 'Standard light C';
    elD55: Result := 'D55';
    elD65: Result := 'D65';
    elD75: Result := 'D75';
    elD50: Result := 'D50';
    elISOStudioTungsten: Result := 'ISO studio tungsten';
    elOther: Result := 'Other';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function MeteringModeToStr(Value: TExifMeteringMode): string;
begin
  case Value of
    emTagMissing: Result := '';
    emUnknown: Result := 'Unknown';
    emAverage: Result := 'Average';
    emCenterWeightedAverage: Result := 'Centre weighted average';
    emSpot: Result := 'Spot';
    emMultiSpot: Result := 'Multiple spot';
    emPattern: Result := 'Pattern';
    emPartial: Result := 'Partial';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function OrientationToStr(Value: TTiffOrientation): string;
begin
  case Value of
    toUndefined: Result := '';
    toTopLeft: Result := 'Normal';
    toTopRight: Result := 'Mirror horizontal';
    toBottomRight: Result := 'Rotate 180°';
    toBottomLeft: Result := 'Mirror vertical';
    toLeftTop: Result := 'Mirrow horizontal and rotate 270°';
    toRightTop: Result := 'Rotate 90°';
    toRightBottom: Result := 'Mirror hotizontal and rotate 90°';
    toLeftBottom: Result := 'Rotate 270°';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function RenderingToStr(Value: TExifRendering): string;
begin
  case Value of
    erTagMissing: Result := '';
    erNormal: Result := 'Normal';
    erCustom: Result := 'Custom';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function ResolutionUnitsToStr(Value: TTiffResolutionUnit): string;
begin
  case Value of
    trNone: Result := '';
    trInch: Result := 'inches';
    trCentimetre: Result := 'centimetres';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SceneCaptureTypeToStr(Value: TExifSceneCaptureType): string;
begin
  case Value of
    ecTagMissing: Result := '';
    ecStandard: Result := 'Standard';
    ecLandscape: Result := 'Landscape';
    ecPortrait: Result := 'Portrait';
    ecNightScene: Result := 'NightScene';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SceneTypeToStr(Value: TExifSceneType): string;
begin
  case Value of
    esUnknown: Result := '';
    esDirectlyPhotographed: Result := 'Directly photographed';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SensingMethodToStr(Value: TExifSensingMethod): string;
begin
  case Value of
    esTagMissing: Result := '';
    esMonochrome: Result := 'Monochrome';
    esOneChip: Result := 'One chip';
    esTwoChip: Result := 'Two chip';
    esThreeChip: Result := 'ThreeChip';
    esColorSequential: Result := 'Colour sequential';
    esTrilinear: Result := 'Trilinear';
    esColorSequentialLinear: Result := 'Colour sequential linear';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function StrobeLightToStr(Value: TExifStrobeLight): string;
begin
  case Value of
    esNoDetectionFunction: Result := 'No detection function';
    esUndetected: Result := 'Undetected';
    esDetected: Result := 'Detected';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function SubjectDistanceRangeToStr(Value: TExifSubjectDistanceRange): string;
begin
  case Value of
    edTagMissing: Result := '';
    edUnknown: Result := 'Unknown';
    edMacro: Result := 'Macro';
    edClose: Result := 'Close';
    edDistant: Result := 'Distant';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function WhiteBalanceModeToStr(Value: TExifWhiteBalanceMode): string;
begin
  case Value of
    ewTagMissing: Result := '';
    ewAuto: Result := 'Auto';
    ewManual: Result := 'Manual';
  else FmtStr(Result, SUnrecognized, [Ord(Value)]);
  end;
end;

function DateTimeToStr(const Value: TDateTime): string;
begin
  DateTimeToString(Result, 'dddddd "at" tt', Value);
end;

end.
