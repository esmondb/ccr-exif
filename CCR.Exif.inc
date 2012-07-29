{.$DEFINE FMX}                //*** remove the dot as desired ***

{$IF CompilerVersion < 23}    //no FMX before XE2
  {$UNDEFINE FMX}
{$IFEND}

{$IFDEF FMX}
  {$UNDEFINE VCL}
{$ELSE}
  {$DEFINE VCL}               //if not FMX, assume VCL
{$ENDIF}

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
