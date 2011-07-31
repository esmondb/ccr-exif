unit CCR.SimpleLinkedList;
{
  Implements a simple generic linked list class. For a 'fuller fat' verion that is 
  comparable to the stock TList<T> and TObjectList<T>, see CCR.LinkedList.pas.
  
  Chris Rolliston, July 2011.
}
interface

type
  TSimpleLinkedList<T> = class
  public type
    TNode = class
    strict private
      FPrevious, FNext: TNode;
      FList: TSimpleLinkedList<T>;
      FValue: T;
    protected
      constructor Create(AList: TSimpleLinkedList<T>; APrevious, ANext: TNode;
        const AValue: T); overload;
    public
      constructor Create; overload; //disallow outside construction by raising an exception
      destructor Destroy; override;
      procedure MoveBefore(ANode: TNode);
      procedure MoveAfter(ANode: TNode);
      property List: TSimpleLinkedList<T> read FList;
      property Next: TNode read FNext write FNext;
      property Previous: TNode read FPrevious write FPrevious;
      property Value: T read FValue write FValue;
    end;

    TValueEnumerator = record
    strict private
      FDoneFirst: Boolean;
      FNode: TNode;
      function GetCurrent: T;
    public
      constructor Create(AHead: TNode);
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  private
    FCount: Integer;
    FFirst, FLast: TNode;
    procedure ValidateNode(ANode: TNode);
  public
    destructor Destroy; override;
    function GetEnumerator: TValueEnumerator;
    procedure Clear;
    function Add(const AValue: T): TNode; inline;
    function AddAfter(ANode: TNode; const AValue: T): TNode;
    function Insert(Index: Integer; const AValue: T): TNode;
    function InsertBefore(ANode: TNode; const AValue: T): TNode;
    property Count: Integer read FCount;
    property First: TNode read FFirst;
    property Last: TNode read FLast;
  end;

implementation

uses SysUtils, RTLConsts; //for the standard exception types and error messages

{ TSimpleLinkedList<T>.TNode }

constructor TSimpleLinkedList<T>.TNode.Create;
begin
  raise ENoConstructException.CreateResFmt(@SNoConstruct, [ClassName]);
end;

constructor TSimpleLinkedList<T>.TNode.Create(AList: TSimpleLinkedList<T>; APrevious, ANext: TNode;
  const AValue: T);
begin
  Assert(AList <> nil);
  inherited Create;
  FPrevious := APrevious;
  FNext := ANext;
  if APrevious <> nil then APrevious.Next := Self;
  if ANext <> nil then ANext.Previous := Self;
  if APrevious = AList.Last then AList.FLast := Self;
  if ANext = AList.First then AList.FFirst := Self;
  FList := AList;
  FValue := AValue;
  Inc(AList.FCount);
end;

destructor TSimpleLinkedList<T>.TNode.Destroy;
begin
  Dec(List.FCount);
  if Self = List.First then List.FFirst := Next;
  if Self = List.Last then List.FLast := Previous;
  if Previous <> nil then Previous.FNext := Next;
  if Next <> nil then Next.FPrevious := Previous;
  inherited Destroy;
end;

procedure TSimpleLinkedList<T>.TNode.MoveBefore(ANode: TNode);
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

procedure TSimpleLinkedList<T>.TNode.MoveAfter(ANode: TNode);
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

{ TSimpleLinkedList<T>.TValueEnumerator }

constructor TSimpleLinkedList<T>.TValueEnumerator.Create(AHead: TNode);
begin
  FDoneFirst := False;
  FNode := AHead;
end;

function TSimpleLinkedList<T>.TValueEnumerator.MoveNext: Boolean;
begin
  if not FDoneFirst then
    FDoneFirst := True
  else
    FNode := FNode.Next;
  Result := (FNode <> nil);
end;

function TSimpleLinkedList<T>.TValueEnumerator.GetCurrent: T;
begin
  Result := FNode.Value;
end;

{ TSimpleLinkedList<T> }

destructor TSimpleLinkedList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSimpleLinkedList<T>.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(First);
end;

function TSimpleLinkedList<T>.Add(const AValue: T): TNode;
begin
  Result := TNode.Create(Self, Last, nil, AValue);
end;

function TSimpleLinkedList<T>.AddAfter(ANode: TNode; const AValue: T): TNode;
begin
  ValidateNode(ANode);
  Result := TNode.Create(Self, ANode, ANode.Next, AValue);
end;

procedure TSimpleLinkedList<T>.Clear;
begin
  while Count > 0 do First.Free;
end;

function TSimpleLinkedList<T>.Insert(Index: Integer; const AValue: T): TNode;
var
  I: Integer;
  Node: TNode;
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Index = Count then
    Result := Add(AValue)
  else
  begin
    Node := First;
    for I := 1 to Index do
      Node := Node.Next;
    Result := TNode.Create(Self, Node.Previous, Node, AValue);
  end;
end;

function TSimpleLinkedList<T>.InsertBefore(ANode: TNode; const AValue: T): TNode;
begin
  ValidateNode(ANode);
  Result := TNode.Create(Self, ANode.Previous, ANode, AValue);
end;

procedure TSimpleLinkedList<T>.ValidateNode(ANode: TNode);
begin
  if ANode = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  if ANode.List <> Self then
    raise EArgumentOutOfRangeException.CreateRes(@SGenericItemNotFound);
end;

end.
