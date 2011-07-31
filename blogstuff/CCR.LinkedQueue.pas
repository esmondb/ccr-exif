unit CCR.LinkedQueue;
{
  Simple TQueue<T> alternative based on a singly-linked list rather than an array.

  Chris Rolliston, July 2011.
}
interface

type
  TLinkedQueue<T> = class
  strict private type
    PNode = ^TNode;
    TNode = record
      NextNode: PNode;
      Value: T;
    end;
  public type
    TEnumerator = record
    strict private
      FDoneFirst: Boolean;
      FNode: PNode;
      function GetCurrent: T;
    public
      constructor Create(AHeadNode: PNode);
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  strict private
    FCount: Integer;
    FHeadNode, FTailNode: PNode;
  public
    destructor Destroy; override;
    function GetEnumerator: TEnumerator;
    procedure Clear;
    procedure Enqueue(const Value: T);
    function Dequeue: T;
    function Peek: T;
    property Count: Integer read FCount;
  end;

implementation

uses RTLConsts, Classes;

constructor TLinkedQueue<T>.TEnumerator.Create(AHeadNode: PNode);
begin
  FDoneFirst := False;
  FNode := AHeadNode;
end;

function TLinkedQueue<T>.TEnumerator.GetCurrent: T;
begin
  Result := FNode.Value;
end;

function TLinkedQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  if FDoneFirst then
    FNode := FNode.NextNode
  else
    FDoneFirst := True;
  Result := (FNode <> nil);
end;

destructor TLinkedQueue<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TLinkedQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FHeadNode);
end;

procedure TLinkedQueue<T>.Clear;
var
  Node: PNode;
begin
  while FHeadNode <> nil do
  begin
    Node := FHeadNode.NextNode;
    Finalize(FHeadNode.Value);
    FreeMem(FHeadNode);
    FHeadNode := Node;
  end;
  FCount := 0;
end;

procedure TLinkedQueue<T>.Enqueue(const Value: T);
var
  NewNode: PNode;
begin
  NewNode := AllocMem(SizeOf(TNode));
  NewNode.Value := Value;
  if FHeadNode = nil then
    FHeadNode := NewNode
  else
    FTailNode.NextNode := NewNode;
  FTailNode := NewNode;
  Inc(FCount);
end;

function TLinkedQueue<T>.Dequeue: T;
var
  Node: PNode;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Node := FHeadNode;
  Result := Node.Value;
  FHeadNode := Node.NextNode;
  Finalize(Node);
  FreeMem(Node);
  Dec(FCount);
end;

function TLinkedQueue<T>.Peek: T;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := FHeadNode.Value;
end;

end.
