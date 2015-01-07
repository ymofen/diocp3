unit u_iocp_api;

interface

{$IF CompilerVersion> 23}
  {$define varNativeUInt}
{$IFEND}

{$if CompilerVersion >= 18}
  {$DEFINE INLINE}
{$IFEND}

uses
  windows, SysUtils;

type
  p_io_response = ^io_response;
  io_response = packed record
    BytesTransferred:ULONG_PTR;
    ResultStatus    :BOOL;
    ErrCode         :Integer;
    CompletionKey   :ULONG_PTR;
  end;

  p_io_request = ^io_request;
  io_request = packed record
    ioResponse       : io_response;
  end;

  POVERLAPPEDEx = ^OVERLAPPEDEx;
  OVERLAPPEDEx = packed record
    Overlapped: OVERLAPPED;
    iocpRequest: p_io_request;
    refCount: Integer;
  end;


  p_io_core = ^io_core;
  io_core = record
    io_handle : NativeUInt;
    function CreateHandle: Boolean;
    procedure PostAIOExitReqeust();
    function bindChildHandle(child_handle:NativeUInt): Integer;
  end;

  p_io_object = ^io_object;
  io_object = record
    io_handle : NativeUInt;
  end;

  io_response_callback = function(pv_io_request:p_io_request):integer;
  p_io_thread_param = ^io_thread_param;
  io_thread_param = record
    ioresponse_callback : io_response_callback;
    iocore   : p_io_core;
  end; 



function bind2Iocp(io_owner_handle, io_handle: NativeUInt; io_completion_key:
    ULONG_PTR): Integer;

function create_iocp_worker(pv_io_thread_param: p_io_thread_param): THandle;

implementation

function bind2Iocp(io_owner_handle, io_handle: NativeUInt; io_completion_key:
    ULONG_PTR): Integer;
begin
  Result := CreateIoCompletionPort(io_handle, io_owner_handle, io_completion_key, 0);
end;


procedure do_iocp_work(param: Pointer); stdcall;
var
  lvBytesTransferred:ULONG_PTR;
  lvResultStatus:BOOL;
  lvErrCode:Integer;
  lpOverlapped: POVERLAPPEDEx;
  lpCompletionKey:ULONG_PTR;
  lvCallback : io_response_callback;
  lvIOCore   : p_io_core;
begin
  lvCallBack := p_io_thread_param(param).ioresponse_callback;
  lvIOCore := p_io_thread_param(param).iocore;
  while true do
  begin
    lvResultStatus := GetQueuedCompletionStatus(lvIOCore.io_handle,
        lvBytesTransferred,  lpCompletionKey,
        POverlapped(lpOverlapped),
        INFINITE);
    if not lvResultStatus then
    begin
      lvErrCode := GetLastError;
    end else
    begin
      lvErrCode := 0;
    end;

    if (lpOverlapped = nil) then
    begin     // exit request
      Break;
    end else if (lpOverlapped.iocpRequest <> nil) then
    begin
      lpOverlapped.iocpRequest.ioResponse.BytesTransferred := lvBytesTransferred;
      lpOverlapped.iocpRequest.ioResponse.ErrCode := lvErrCode;
      lpOverlapped.iocpRequest.ioResponse.CompletionKey := lpCompletionKey;
      lpOverlapped.iocpRequest.ioResponse.ResultStatus := lvResultStatus;  
      lvCallBack(lpOverlapped.iocpRequest);
    end else
    begin
      Break;
    end;   
  end;
end;

function create_iocp_worker(pv_io_thread_param: p_io_thread_param): THandle;
var
  lvHandle:THandle;
begin
  if not IsMultiThread then IsMultiThread := true;
  lvHandle := 0;
  CreateThread(0, 0, @do_iocp_work, pv_io_thread_param, 0, lvHandle);
  result := lvHandle;
end;

function io_core.bindChildHandle(child_handle:NativeUInt): Integer;
begin
  Result := bind2Iocp(io_handle, child_handle, 0);
end;

function io_core.CreateHandle: Boolean;
begin
  io_handle := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  Result := (io_handle <> 0) and (io_handle <> INVALID_HANDLE_VALUE);
end;

procedure io_core.PostAIOExitReqeust;
begin
  PostQueuedCompletionStatus(io_handle, 0, 0, nil);
end;

end.
