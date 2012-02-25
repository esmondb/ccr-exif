{.$DEFINE FMX}                //*** remove the dot as desired ***
{.$DEFINE VCL}                //*** remove the dot as desired ***

{$IF CompilerVersion < 23}
  {$DEFINE VCL}
{$IFEND}

{$IF CompilerVersion >= 18.5}
  {$DEFINE CanInline}         //inlining record methods is unstable before D2007
{$IFEND}

{$IF CompilerVersion >= 20}
  {$DEFINE DepCom}            //deprecated directive allows comments from D2009
  {$DEFINE HasToString}
{$IFEND}

{$IF CompilerVersion >= 21}
  {$DEFINE NoRecEnumBug}      //QC 72213
  {$DEFINE HasIOUtils}
{$IFEND}

{$IF CompilerVersion >= 22}
  {$DEFINE HasTTimeZone}
{$IFEND}

{$IFOPT Q+}
  {$DEFINE OverflowCheckingOn}
{$ENDIF}

{$IFOPT R-}
  {$DEFINE RangeCheckingOff}
{$ENDIF}

{$IF CompilerVersion < 23}
  {$UNDEF FMX}                //no FMX pre-XE2
{$IFEND}

{$IFNDEF MSWINDOWS}
  {.$DEFINE FMX}               //'force' FMX if not a Windows target
{$ENDIF}

{$IFDEF FMX}
  {$UNDEF VCL}
{$ELSE}
  {.$DEFINE VCL}               //if no FMX, then assume VCL
{$ENDIF}
