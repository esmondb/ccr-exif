unit CCR.LinkedList;
{
  Implements a linked list class broadly in line with both the stock, array-based
  TList and the .NET LinkedList class. The main interface differences from the former
  are the following:
  - No indexer (the .NET class doesn't have one either). Instead of indices, there are
    nodes, objects for which are exposed: to delete an item, you therefore free a node.
  - FindNode and FindLastNode instead of IndexOf and LastIndexOf.
  - Doesn't implement Sort and BinarySearch.
  - No OnNotify event.
  - No separate TObjectList-type descendant - instead, (optional) object ownership is
    implemented in TLinkedList itself.
	
  Chris Rolliston, July 2011.
}
interface

uses
  SysUtils, Generics.Collections, Generics.Defaults;

type
  TLinkedList<T> = class(TEnumerable<T>)
  public type
    TNode = class
    strict private
      FPrevious, FNext: TNode;
      FList: TLinkedList<T>;
      FValue: T;
      procedure SetValue(const NewValue: T);
    protected
      constructor Create(AList: TLinkedList<T>; APrevious, ANext: TNode;
        const AValue: T); overload;
    public
      constructor Create; overload; //disallow outside construction by raising an exception
      destructor Destroy; override;
      procedure ExchangeValues(ANode: TNode);
      function ExtractValue: T;
      procedure MoveBefore(ANode: TNode);
      procedure MoveAfter(ANode: TNode);
      function IsFirst: Boolean;
      function IsLast: Boolean;
      property List: TLinkedList<T> read FList;
      property Next: TNode read FNext write FNext;
      property Previous: TNode read FPrevious write FPrevious;
      property Value: T read FValue write SetValue;
    end;

    TNodeEnumerator = record
    strict private
      FDoneFirst: Boolean;
      FNode: TNode;
    public
      constructor Create(AHead: TNode);
      function MoveNext: Boolean;
      property Current: TNode read FNode;
    end;

    TNodes = record
    private
      FFirst, FLast: TNode;
      FList: TLinkedList<T>;
    public
      function GetEnumerator: TNodeEnumerator;
      property First: TNode read FFirst;
      property Last: TNode read FLast;
    end;

    TValueEnumerator = class(TEnumerator<T>)
    strict private
      FDoneFirst: Boolean;
      FNode: TNode;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(AHead: TNode);
    end;
  private
    FComparer: IComparer<T>;
    FCount: Integer;
    FNodes: TNodes;
    FOwnsObjects: Boolean;
    procedure SetOwnsObjects(Value: Boolean);
    procedure ValidateNode(ANode: TNode);
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(Collection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Clear;
    function Add(const AValue: T): TNode;
    function AddAfter(ANode: TNode; const AValue: T): TNode;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;
    procedure AddRangeAfter(ANode: TNode; const Values: array of T); overload;
    procedure AddRangeAfter(ANode: TNode; ACollection: TEnumerable<T>); overload;
    function Contains(const Value: T): Boolean;
    procedure Exchange(ANode1, ANode2: TNode);
    function Extract(const Value: T): T;
    function FindNode(const Value: T; out Node: TNode): Boolean;
    function FindLastNode(const Value: T; out Node: TNode): Boolean;
    function First: T;
    function Last: T;
    function Insert(Index: Integer; const AValue: T): TNode;
    function InsertBefore(ANode: TNode; const AValue: T): TNode;
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; Collection: TEnumerable<T>); overload;
    procedure InsertRangeBefore(ANode: TNode; const Values: array of T); overload;
    procedure InsertRangeBefore(ANode: TNode; ACollection: TEnumerable<T>); overload;
    function Remove(const Value: T): Boolean;
    procedure Reverse;
    function ToArray: TArray<T>; override;
    property Count: Integer read FCount;
    property Nodes: TNodes read FNodes;
    property OwnsObjects: Boolean read FOwnsObjects write SetOwnsObjects;
  end;

implementation

uses TypInfo, SysConst, RTLConsts;

{ TLinkedList<T>.TNode }

constructor TLinkedList<T>.TNode.Create;
begin
  raise ENoConstructException.CreateResFmt(@SNoConstruct, [ClassName]);
end;

constructor TLinkedList<T>.TNode.Create(AList: TLinkedList<T>; APrevious, ANext: TNode;
  const AValue: T);
begin
  Assert(AList <> nil);
  inherited Create;
  FPrevious := APrevious;
  FNext := ANext;
  if APrevious <> nil then APrevious.Next := Self;
  if ANext <> nil then ANext.Previous := Self;
  if APrevious = AList.Nodes.Last then AList.FNodes.FLast := Self;
  if ANext = AList.Nodes.First then AList.FNodes.FFirst := Self;
  FList := AList;
  FValue := AValue;
  Inc(AList.FCount);
end;

destructor TLinkedList<T>.TNode.Destroy;
begin
  if List.OwnsObjects then PObject(@Value)^.Free;
  Dec(List.FCount);
  if Self = List.Nodes.First then List.FNodes.FFirst := Next;
  if Self = List.Nodes.Last then List.FNodes.FLast := Previous;
  if Previous <> nil then Previous.FNext := Next;
  if Next <> nil then Next.FPrevious := Previous;
  inherited Destroy;
end;

procedure TLinkedList<T>.TNode.ExchangeValues(ANode: TNode);
var
  Temp: T;
begin
  if ANode = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  Temp := FValue;
  FValue := ANode.Value;
  ANode.FValue := Temp;
end;

function TLinkedList<T>.TNode.ExtractValue: T;
begin
  Result := FValue;
  FValue := Default(T);
end;

procedure TLinkedList<T>.TNode.MoveBefore(ANode: TNode);
begin
  List.ValidateNode(ANode);
  if ANode = Next then Exit;
  //unlink ourselves from our current neighbours
  if Previous <> nil then Previous.Next := Next;
  if Next <> nil then Next.Previous := Previous;
  //link ourselves to our new neighbours
  Previous := ANode.Previous;
  if Previous <> nil then Previous.Next := Self;
  ANode.Previous := Self;
  Next := ANode;
end;

procedure TLinkedList<T>.TNode.MoveAfter(ANode: TNode);
begin
  List.ValidateNode(ANode);
  if ANode = Previous then Exit;
  //unlink ourselves from our current neighbours
  if Previous <> nil then Previous.Next := Next;
  if Next <> nil then Next.Previous := Previous;
  //link ourselves to our new neighbours
  Next := ANode.Next;
  if Next <> nil then Next.Previous := Self;
  ANode.Next := Self;
  Previous := ANode;
end;

function TLinkedList<T>.TNode.IsFirst: Boolean;
begin
  Result := (Previous = nil)
end;

function TLinkedList<T>.TNode.IsLast: Boolean;
begin
  Result := (Next = nil)
end;

procedure TLinkedList<T>.TNode.SetValue(const NewValue: T);
begin
  if List.OwnsObjects then
  begin
    if PObject(@NewValue)^ = PObject(@Value)^ then Exit;
    PObject(@Value)^.Free;
  end;
  FValue := NewValue;
end;

{ TLinkedList<T>.TNodeEnumerator }

constructor TLinkedList<T>.TNodeEnumerator.Create(AHead: TNode);
begin
  FDoneFirst := False;
  FNode := AHead;
end;

function TLinkedList<T>.TNodeEnumerator.MoveNext: Boolean;
begin
  if not FDoneFirst then
    FDoneFirst := True
  else
    FNode := FNode.Next;
  Result := (FNode <> nil);
end;

{ TLinkedList<T>.TNodes }

function TLinkedList<T>.TNodes.GetEnumerator: TNodeEnumerator;
begin
  Result := TNodeEnumerator.Create(First);
end;

{ TLinkedList<T>.TValueEnumerator }

constructor TLinkedList<T>.TValueEnumerator.Create(AHead: TNode);
begin
  inherited Create;
  FDoneFirst := False;
  FNode := AHead;
end;

function TLinkedList<T>.TValueEnumerator.DoMoveNext: Boolean;
begin
  if not FDoneFirst then
    FDoneFirst := True
  else
    FNode := FNode.Next;
  Result := (FNode <> nil);
end;

function TLinkedList<T>.TValueEnumerator.DoGetCurrent: T;
begin
  Result := FNode.Value;
end;

{ TLinkedList<T> }

constructor TLinkedList<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TLinkedList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TComparer<T>.Default;
  FNodes.FList := Self;
end;

constructor TLinkedList<T>.Create(Collection: TEnumerable<T>);
begin
  Create;
  AddRange(Collection);
end;

destructor TLinkedList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLinkedList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := TValueEnumerator.Create(Nodes.First);
end;

function TLinkedList<T>.Add(const AValue: T): TNode;
begin
  Result := TNode.Create(Self, Nodes.Last, nil, AValue);
end;

function TLinkedList<T>.AddAfter(ANode: TNode; const AValue: T): TNode;
begin
  ValidateNode(ANode);
  Result := TNode.Create(Self, ANode, ANode.Next, AValue);
end;

procedure TLinkedList<T>.AddRange(const Values: array of T);
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    Add(Values[I]);
end;

procedure TLinkedList<T>.AddRange(Collection: TEnumerable<T>);
var
  Value: T;
begin
  for Value in Collection do
    Add(Value);
end;

procedure TLinkedList<T>.AddRangeAfter(ANode: TNode; const Values: array of T);
var
  I: Integer;
begin
  ValidateNode(ANode);
  for I := Low(Values) to High(Values) do
    ANode := TNode.Create(Self, ANode, ANode.Next, Values[I]);
end;

procedure TLinkedList<T>.AddRangeAfter(ANode: TNode; ACollection: TEnumerable<T>);
var
  Value: T;
begin
  ValidateNode(ANode);
  for Value in ACollection do
    ANode := TNode.Create(Self, ANode, ANode.Next, Value);
end;

procedure TLinkedList<T>.Clear;
begin
  while Count > 0 do Nodes.First.Free;
end;

function TLinkedList<T>.Contains(const Value: T): Boolean;
var
  Node: TNode;
begin
  Result := FindNode(Value, Node);
end;

procedure TLinkedList<T>.Exchange(ANode1, ANode2: TNode);
begin
  ValidateNode(ANode1);
  ValidateNode(ANode2);
  ANode1.ExchangeValues(ANode2);
end;

function TLinkedList<T>.Extract(const Value: T): T;
var
  Node: TNode;
begin
  if not FindNode(Value, Node) then Exit(Default(T));
  Result := Node.ExtractValue;
  Node.Free;
end;

function TLinkedList<T>.FindNode(const Value: T; out Node: TNode): Boolean;
begin
  Node := Nodes.First;
  while Node <> nil do
  begin
    if FComparer.Compare(Node.Value, Value) = 0 then Exit(True);
    Node := Node.Next;
  end;
  Result := False;
end;

function TLinkedList<T>.FindLastNode(const Value: T; out Node: TNode): Boolean;
begin
  Node := Nodes.Last;
  while Node <> nil do
  begin
    if FComparer.Compare(Node.Value, Value) = 0 then Exit(True);
    Node := Node.Previous;
  end;
  Result := False;
end;

function TLinkedList<T>.First: T;
begin
  if Count = 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := Nodes.First.Value;
end;

function TLinkedList<T>.Last: T;
begin
  if Count = 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := Nodes.Last.Value;
end;

function TLinkedList<T>.Insert(Index: Integer; const AValue: T): TNode;
var
  I: Integer;
  Node: TNode;
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Index = 0 then
    Result := TNode.Create(Self, nil, Nodes.First, AValue)
  else if Index = Count then
    Result := Add(AValue)
  else
  begin
    Node := Nodes.First;
    for I := 1 to Index do
      Node := Node.Next;
    Result := TNode.Create(Self, Node.Previous, Node, AValue);
  end;
end;

function TLinkedList<T>.InsertBefore(ANode: TNode; const AValue: T): TNode;
begin
  ValidateNode(ANode);
  Result := TNode.Create(Self, ANode.Previous, ANode, AValue);
end;

procedure TLinkedList<T>.InsertRange(Index: Integer; const Values: array of T);
var
  I: Integer;
  NewNode: TNode;
begin
  if Length(Values) = 0 then Exit;
  NewNode := Insert(Index, Values[0]);
  for I := 1 to High(Values) do
    NewNode := AddAfter(NewNode, Values[I]);
end;

procedure TLinkedList<T>.InsertRange(Index: Integer; Collection: TEnumerable<T>);
var
  NewNode: TNode;
  Value: T;
begin
  NewNode := nil;
  for Value in Collection do
    if NewNode = nil then
      NewNode := Insert(Index, Value)
    else
      NewNode := AddAfter(NewNode, Value);
end;

procedure TLinkedList<T>.InsertRangeBefore(ANode: TNode; const Values: array of T);
var
  I: Integer;
begin
  for I := High(Values) downto Low(Values) do
    ANode := InsertBefore(ANode, Values[I]);
end;

procedure TLinkedList<T>.InsertRangeBefore(ANode: TNode; ACollection: TEnumerable<T>);
var
  Cursor: TNode;
  Value: T;
begin
  Cursor := nil;
  for Value in ACollection do
    if Cursor = nil then
      Cursor := InsertBefore(ANode, Value)
    else
      Cursor := AddAfter(Cursor, Value);
end;

function TLinkedList<T>.Remove(const Value: T): Boolean;
var
  Node: TNode;
begin
  Result := FindNode(Value, Node);
  if Result then Node.Free;
end;

procedure TLinkedList<T>.Reverse;
var
  A, B: TNode;
begin
  if Count < 2 then Exit;
  A := Nodes.First;
  B := Nodes.Last;
  repeat
    A.ExchangeValues(B);
    A := A.Next;
    if A = B then Break;
    B := B.Previous;
  until B = A;
end;

procedure TLinkedList<T>.SetOwnsObjects(Value: Boolean);
begin
  if Value = FOwnsObjects then Exit;
  if PTypeInfo(TypeInfo(T)).Kind <> tkClass then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  FOwnsObjects := Value;
end;

function TLinkedList<T>.ToArray: TArray<T>;
var
  I: Integer;
  Node: TNode;
begin
  SetLength(Result, Count);
  Node := Nodes.First;
  for I := 0 to Count - 1 do
  begin
    Result[I] := Node.Value;
    Node := Node.Next;
  end;
end;

procedure TLinkedList<T>.ValidateNode(ANode: TNode);
begin
  if ANode = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  if ANode.List <> Self then
    raise EArgumentOutOfRangeException.CreateRes(@SGenericItemNotFound);
end;

end.
