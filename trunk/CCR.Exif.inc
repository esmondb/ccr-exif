{$IF CompilerVersion >= 18.5}
  {$DEFINE CanInline} //inlining record methods is unstable before D2007
{$IFEND}

{$IF CompilerVersion >= 20}
  {$DEFINE DEPCON}
{$IFEND}

{$IF CompilerVersion >= 21}
  {$DEFINE NoRecEnumBug} //QC 72213
  {$DEFINE HasIOUtils}
{$IFEND}

{$IFOPT Q+}
  {$DEFINE OverflowCheckingOn}
{$ENDIF}

{$IFOPT R-}
  {$DEFINE RangeCheckingOff}
{$ENDIF}   

