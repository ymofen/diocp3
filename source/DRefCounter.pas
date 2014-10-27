unit DRefCounter;

interface

uses
  Windows;


type
  TObjectNotifyEvent = procedure(Sender: TObject) of object;
  TDRefCounter = class(TObject)
  private
    FOnRelease: TObjectNotifyEvent;
    FRefCounter: Integer;
  public
    constructor Create;
    function decReference: Integer;
    function incReference: Integer;
    property OnRelease: TObjectNotifyEvent read FOnRelease write FOnRelease;
  end;

implementation

constructor TDRefCounter.Create;
begin
  inherited Create;
  FRefCounter := 0;
end;

function TDRefCounter.decReference: Integer;
begin
  Result := InterlockedDecrement(FRefCounter);
  if Result = 0 then
  begin
    if Assigned(FOnRelease) then FOnRelease(Self)
  end;
end;

function TDRefCounter.incReference: Integer;
begin
  Result := InterlockedIncrement(FRefCounter);
end;

end.
