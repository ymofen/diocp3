(*
  unit owner:
    d10.ymofen, qdac.swish
  + first
    2014-10-10 12:45:23
*)
unit DHashTable;

interface

uses
  SysUtils;


type
{$IF RTLVersion<25}
  IntPtr = Integer;
{$IFEND}

  EDHashTableException = Class(Exception);
  /// <summary>
  ///   hash value type
  /// </summary>
  TDHashValueType = Cardinal;

  PDHashData=^TDHashData;
  TDHashData=record
    Next       : PDHashData;      // next value
    Data       : Pointer;         // data
    Hash       : TDHashValueType; // data hash value
  end;

  TDBuckets =array of PDHashData;

  TOnDataCompare = function(P1,P2:Pointer): Integer of object;
  TOnDHashDataNotify = procedure(pvData:PDHashData) of object;
  TOnDataNotify = procedure(pvData:Pointer) of object;

  TDHashTable = class(TObject)
  private
    FOnCompare: TOnDataCompare;

    FBucketSize: Cardinal;

    FCount:Integer;

    FBuckets:TDBuckets;
    FOnDelete: TOnDataNotify;


    procedure DoDelete(AHash:TDHashValueType; AData:Pointer);

    procedure CreateHashData(var vData: PDHashData);
    function GetBuckets(AIndex: Cardinal): PDHashData;

    procedure ReleaseHashData(var vData: PDHashData);

    function InnerCompare(pvData1, pvData2:Pointer): Integer;

    procedure SetOnCompare(const Value: TOnDataCompare);

  public
    constructor Create(pvBucketSize: Cardinal = 1361);

    /// <summary>
    ///   for each element and invoke callback proc
    /// </summary>
    procedure ForEach(pvCallback:TOnDHashDataNotify);

    /// <summary>
    ///  add AData
    /// </summary>
    procedure Add(pvHashValue: TDHashValueType; pvData: Pointer);

    /// <summary>
    ///   find first item by hashValue
    /// </summary>
    function FindFirst(pvHashValue:TDHashValueType): PDHashData;


    /// <summary>
    ///   find first data by hashValue
    /// </summary>
    function FindFirstData(pvHashValue:TDHashValueType): Pointer;

    /// <summary>
    ///   clear all data
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   delete frist element by hashValue
    /// </summary>
    function DeleteFirst(pvHashValue: TDHashValueType; pvData: Pointer): Boolean; overload;

    /// <summary>
    ///   delete frist element by hashValue
    /// </summary>
    function DeleteFirst(pvHashValue: TDHashValueType): Boolean; overload;


    /// <summary>
    ///   exists?
    /// </summary>
    function Exists(pvHashValue: TDHashValueType; pvData: Pointer): Boolean;overload;

    /// <summary>
    ///   exists?
    /// </summary>
    function Exists(pvHashValue: TDHashValueType): Boolean; overload;


    /// <summary>
    ///   resize bucket length
    /// </summary>
    procedure SetBucketSize(pvBucketSize:Integer);

    property Buckets[AIndex: Cardinal]: PDHashData read GetBuckets;

    property BucketSize: Cardinal read FBucketSize;

    property Count: Integer read FCount;

    property OnDelete: TOnDataNotify read FOnDelete write FOnDelete;

    property OnCompare: TOnDataCompare read FOnCompare write SetOnCompare;

  end;

implementation

resourcestring
  SHashTableIndexError = 'Buckets index out of bounds (%d)';

procedure TDHashTable.Clear;
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      FBuckets[I]:=lvBucket.Next;

      DoDelete(lvBucket.Hash, lvBucket.Data);
      ReleaseHashData(lvBucket);

      lvBucket:=FBuckets[I];
    end;
  end;
  FCount:=0;
end;

constructor TDHashTable.Create(pvBucketSize: Cardinal = 1361);
begin
  inherited Create;
  SetBucketSize(pvBucketSize);
  FOnCompare := InnerCompare;
end;

procedure TDHashTable.Add(pvHashValue: TDHashValueType; pvData: Pointer);
var
  lvIndex :Cardinal;
  lvBucket:PDHashData;
begin
  CreateHashData(lvBucket);

  lvBucket.Data:=pvData;
  lvBucket.Hash:=pvHashValue;

  lvIndex := pvHashValue mod FBucketSize;
  lvBucket.Next:=FBuckets[lvIndex];

  FBuckets[lvIndex]:=lvBucket;

  Inc(FCount);
end;

function TDHashTable.InnerCompare(pvData1, pvData2:Pointer): Integer;
begin           
  Result := IntPtr(pvData1) - IntPtr(pvData2);
end;

procedure TDHashTable.CreateHashData(var vData: PDHashData);
begin
  New(vData);
end;

function TDHashTable.DeleteFirst(pvHashValue: TDHashValueType): Boolean;
var
  lvIndex:Cardinal;
  lvCurrData, lvPrior:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];
  lvPrior:=nil;

  while Assigned(lvCurrData) do
  begin
    if lvCurrData.Hash = pvHashValue then
    begin
      if Assigned(lvPrior) then
        lvPrior.Next := lvCurrData.Next
      else
        FBuckets[lvIndex]:= lvCurrData.Next;

      DoDelete(lvCurrData.Hash, lvCurrData.Data);

      ReleaseHashData(lvCurrData);
      Dec(FCount);
      Result := true;
      Break;
    end else
    begin
      lvPrior:= lvCurrData;
      lvCurrData:=lvPrior.Next;
    end;
  end;
end;

procedure TDHashTable.DoDelete(AHash:TDHashValueType; AData:Pointer);
begin
  if Assigned(FOnDelete) then
    FOnDelete(AData);

end;

function TDHashTable.Exists(pvHashValue: TDHashValueType; pvData: Pointer):
    Boolean;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //first compare hash value
    if lvCurrData.Hash = pvHashValue then
      if FOnCompare(pvData, lvCurrData.Data) = 0 then
      begin
        Result := true;
        Break;
      end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

function TDHashTable.FindFirst(pvHashValue:TDHashValueType): PDHashData;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := nil;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      Result := lvCurrData;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

function TDHashTable.FindFirstData(pvHashValue:TDHashValueType): Pointer;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := nil;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      Result := lvCurrData.Data;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

procedure TDHashTable.ForEach(pvCallback:TOnDHashDataNotify);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  Assert(Assigned(pvCallback));
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvCallback(lvBucket);
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

function TDHashTable.GetBuckets(AIndex: Cardinal): PDHashData;
begin
  if (AIndex>=FBucketSize) then
  begin
    raise EDHashTableException.CreateFmt(SHashTableIndexError, [AIndex]);
  end;

  Result := FBuckets[AIndex];
end;

procedure TDHashTable.ReleaseHashData(var vData: PDHashData);
begin
  Dispose(vData);
end;

function TDHashTable.DeleteFirst(pvHashValue: TDHashValueType; pvData:
    Pointer): Boolean;
var
  lvIndex:Cardinal;
  lvCurrData, lvPrior:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];
  lvPrior:=nil;
  
  while Assigned(lvCurrData) do
  begin
    if FOnCompare(pvData, lvCurrData.Data) = 0 then
    begin
      if Assigned(lvPrior) then
        lvPrior.Next := lvCurrData.Next
      else
        FBuckets[lvIndex]:= lvCurrData.Next;

      DoDelete(lvCurrData.Hash, lvCurrData.Data);
      
      ReleaseHashData(lvCurrData);
      Dec(FCount);
      Result := true;
      Break;
    end else
    begin
      lvPrior:= lvCurrData;
      lvCurrData:=lvPrior.Next;
    end;
  end;   
end;

function TDHashTable.Exists(pvHashValue: TDHashValueType): Boolean;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      Result := true;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

procedure TDHashTable.SetBucketSize(pvBucketSize:Integer);
const
  //default bucket size
  BucketNormalSize:array[0..27] of Integer=(
    17,37,79,163,331,673, 1361, 2729, 5471,10949,21911,43853,87719,175447,350899,
    701819,1403641,2807303,5614657,11229331,22458671,44917381,89834777,
    179669557,359339171,718678369,1437356741,2147483647
    );
var
  lvIndex, lvBucketSize:Cardinal;
  I :Integer;
  lvHash  : TDHashValueType;
  lvOldBuckets: TDBuckets;
  lvData, lvNext: PDHashData;
begin
  lvBucketSize := pvBucketSize;
  if lvBucketSize=0 then
  begin
    for i:=0 to 27 do
    begin
      if BucketNormalSize[i] > FCount then
      begin
        lvBucketSize:= BucketNormalSize[i];
        Break;
      end;
    end;

    if lvBucketSize=0 then  // max size
      lvBucketSize:= BucketNormalSize[27];
    if lvBucketSize = FBucketSize then Exit;
  end;

  if FBucketSize <> lvBucketSize then
  begin   // bucket size changed

    // save old arrange
    lvOldBuckets := FBuckets;

    // new bucket size
    FBucketSize := lvBucketSize;
    SetLength(FBuckets, FBucketSize);

    // empty
    for I := 0 to FBucketSize - 1 do FBuckets[I]:=nil;

    
    // rearrange element
    for I := 0 to High(lvOldBuckets) do
    begin        
      lvData:=lvOldBuckets[I];
      while lvData<>nil do
      begin
        lvHash := lvData.Hash;
        lvIndex := lvHash mod FBucketSize;
        lvNext := lvData.Next;

        lvData.Next := FBuckets[lvIndex];
        FBuckets[lvIndex]:=lvData;
        lvData := lvNext;
      end;
    end;
  end;
end;

procedure TDHashTable.SetOnCompare(const Value: TOnDataCompare);
begin
  if not Assigned(Value) then
    FOnCompare := InnerCompare
  else
    FOnCompare := Value;
end;


end.
