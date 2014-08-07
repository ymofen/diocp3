unit iocpLocker;

interface

{$DEFINE USECriticalSection}

uses
  {$IFDEF USECriticalSection}
    SyncObjs,
  {$ELSE}
    Windows,
  {$ENDIF}
  SysUtils, Classes;

type
  {$IFDEF USECriticalSection}
    TIocpLocker = class(TCriticalSection)
  {$ELSE}
    TIocpLocker = class(TObject)
  {$ENDIF}
  private
    FEnterINfo: string;
    FName: String;

  {$IFDEF USECriticalSection}
  {$ELSE}
    FSection: TRTLCriticalSection;
  {$ENDIF}

    function GetEnterCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure lock;
    procedure unLock;

    property EnterCount: Integer read GetEnterCount;
    property EnterINfo: string read FEnterINfo write FEnterINfo;
    property Name: String read FName write FName;

  end;

implementation

constructor TIocpLocker.Create;
begin
  inherited Create;
  {$IFDEF USECriticalSection}

  {$ELSE}
    InitializeCriticalSection(FSection);
  {$ENDIF}
end;

destructor TIocpLocker.Destroy;
begin
  {$IFDEF USECriticalSection}

  {$ELSE}
    DeleteCriticalSection(FSection);
  {$ENDIF}

  inherited Destroy;
end;

function TIocpLocker.GetEnterCount: Integer;
begin
  {$IFDEF USECriticalSection}
     Result := 0;
  {$ELSE}
     Result := FSection.RecursionCount;
  {$ENDIF}

end;

procedure TIocpLocker.lock;
begin
  {$IFDEF USECriticalSection}
    Enter;
  {$ELSE}
     EnterCriticalSection(FSection);
  {$ENDIF}
end;

procedure TIocpLocker.unLock;
begin
  {$IFDEF USECriticalSection}
     Leave;
  {$ELSE}
     LeaveCriticalSection(FSection);
  {$ENDIF}

  //OutputDebugString(PChar(Format('%d:%s unlock', [GetCurrentThreadID, Name])));
  //FEnterINfo := '';
end;

end.
