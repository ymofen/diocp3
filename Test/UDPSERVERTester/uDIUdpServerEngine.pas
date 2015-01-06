unit uDIUdpServerEngine;

interface

uses
  Windows,
  Sysutils,
  WinSock2,
  uDIBuffer,
  uDIProtocol,
  uDIMapBuffer,
  uDIPoolBuffer,
  uDISocketEngine,
  uIOCompletionPort;
  {$I IOCP.inc}
  
type

  TDIUdpServerEngine = class(TDISocketEngine)
  private
    m_dLastTime:            DWORD;  // 记录时间
    m_iInitPostRecvBuffer:  DWORD;  // 保持接收的Buffer数量
  public
    // 分配Buffer
    function AllocateBuffer: TDIBuffer; override;
    // 回收Buffer
    function ReleaseBuffer(FBuffer: TDIBuffer; m_bAcceptEx: Boolean = FALSE): Boolean; override;
    // 释放所有Buffer
    procedure FreeAllBuffers;
    // 释放系统内存资源
    procedure FreeAllPhysicsMemory; override;
  protected
    // 创建套接字, 接受数据
    function ListnerStart: Boolean;
  public
    // 以下是消息处理函数
    // 投递Recv I/O 消息
    function PostWSARecvEvent(FDIClientContext: TDIClientContext; FRecvBuffer: TDIBuffer): Boolean; override;
    // 处理投递的Recv I/O 消息
    function OnWSARecv(FDIClientContext: TDIClientContext; FRecvBuffer: TDIBuffer; dwIoSize: DWORD): Boolean;

    // 投递Send I/O 消息
    function PostWSASendEvent(FDIClientContext: TDIClientContext; FSendBuffer: TDIBuffer): Boolean; override;
    // 处理投递的Send I/O 消息
    function OnWSASend(FDIClientContext: TDIClientContext; FSendBuffer: TDIBuffer; dwIoSize: DWORD): Boolean;

 public
    procedure ProcessIOError(FIOEventType: IO_POST_EVENT_TYPE; FDIClientContext: TDIClientContext); override;
    procedure ProcessIOMessage(FIOEventType: IO_POST_EVENT_TYPE; FDIClientContext: TDIClientContext; FBuffer: TDIBuffer; dwIoSize: DWORD); override;

 public
    constructor Create(IOCompletionPort: TIOCompletionPort);
    destructor Destroy; override;
    function StartServer( nPort: Integer;
                          iMaxNumConnections: DWORD;
                          iMaxNumberOfFreeContext: DWORD;
                          iMaxNumberOfFreeBuffer: DWORD;
                          StartEngine: TENGINE_START_TYPE = SERVER_ENGINE_START ): Boolean;  override;
		procedure StopServer; override;
  end;

implementation
  uses uDIResourceStr, uGlobalLogger, uIOCPMonitor;
  

// TDIUdpServerEngine
constructor TDIUdpServerEngine.Create(IOCompletionPort: TIOCompletionPort);
begin
  inherited Create;
  m_IOCompletionPort := IOCompletionPort;
end;

destructor TDIUdpServerEngine.Destroy;
begin
  inherited Destroy;
end;

function TDIUdpServerEngine.ListnerStart: Boolean;
var
  I: Integer;
  FRecvBuffer: TDIBuffer;
begin
  if not FWinSocket.CreateUDPOverlapSocket(m_sListenSocket) then begin

    {$IFDEF _ICOP_DEBUGERR}
        _GlobalLogger.AppendErrorLogMessage('创建 Socket监听套按字失败: %d.', [WSAGetLastError()]);
    {$ENDIF}
    Result := FALSE;
    Exit;
  end;

  if not FWinSocket.SetSocketReUseAddr(m_sListenSocket) then begin
  
    {$IFDEF _ICOP_DEBUGERR}
        _GlobalLogger.AppendErrorLogMessage('设置端口复用setsockopt(SO_EXCLUSIVEADDRUSE) 创建失败: %d.', [WSAGetLastError()]);
    {$ENDIF}
    FWinSocket.CloseWinSocket(m_sListenSocket);
    Result := FALSE;
    Exit;
  end;

  // 绑定套接字
  if not FWinSocket.BindSocket(m_sListenSocket, m_nPort) then begin

    {$IFDEF _ICOP_DEBUGERR}
        _GlobalLogger.AppendErrorLogMessage('绑定套接字 bind() 调用失败: %d.', [WSAGetLastError()]);
    {$ENDIF}
    Result := FALSE;
    Exit;
  end;

  // 设置大小
  FWinSocket.SetUDPSocketSize(m_sListenSocket, 1024*40);

  // 将监听套节字关联到完成端口，注意，这里为它传递的CompletionKey为0
  m_IOCompletionPort.AssociateSocketWithCompletionPort(m_sListenSocket, DWORD(0));

  // 投递Recv /IO
  for I:=1 to m_iMaxNumConnections-1 do begin
    FRecvBuffer := AllocateBuffer;

    if FRecvBuffer<>nil then begin
      FRecvBuffer.m_Socket := m_sListenSocket;
      PostWSARecv(FRecvBuffer.m_Socket, FRecvBuffer, PROTOOOL_UDP);
    end;
  end;
  
  // 调置允许连接 计算超时时间
  m_bAcceptConnections := TRUE;
  m_dLastTime := GetTickCount;
	Result := TRUE;
end;

function TDIUdpServerEngine.AllocateBuffer: TDIBuffer;
var
  pBuffer: TDIBuffer;
begin
  Result := nil;

  pBuffer := FPoolDIBuffer.AllocateFreeBufferFromPool;
  if (pBuffer <>nil) then begin
    if FMapBuffer.AddDIBuffer(pBuffer) then begin
    
      {$IFDEF _ICOP_DEBUG}
          _GlobalLogger.AppendErrorLogMessage('AllocateBuffer MapID is %d', [pBuffer.m_MapID]);
      {$ENDIF}
      Result := pBuffer;
      Exit;
    end
    else
    begin
      FPoolDIBuffer.ReleaseBufferToPool(pBuffer);
      {$IFDEF _ICOP_DEBUGERR}
          _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_ALLOCATE_BUFFER, []);
      {$ENDIF}
    end;
  end;
end;

function TDIUdpServerEngine.ReleaseBuffer(FBuffer: TDIBuffer; m_bAcceptEx: Boolean = FALSE): Boolean;  
{$IFDEF _ICOP_DEBUGERR}
var
  sMsg: String;
{$ENDIF}
begin
  Result := FALSE;
  if Assigned(FBuffer) then begin

    {$IFDEF _ICOP_DEBUGERR}
       case FBuffer.GetOperation of
         IO_INITIALIZE:      sMsg := 'IO_INITIALIZE';
         IO_WSA_ACCEPTEX:    sMsg := 'IO_WSA_ACCEPTEX';
         IO_WSA_RECV:        sMsg := 'IO_WSA_RECV';
         IO_WSA_SEND:        sMsg := 'IO_WSA_SEND';
         IO_WSA_CLOSESOCKET: sMsg := 'IO_WSA_CLOSESOCKET';
       end;
    {$ENDIF}

    Result := FMapBuffer.RemoveDIBuffer(FBuffer);
    
    {$IFDEF _ICOP_DEBUG}
      if not Result then
         _GlobalLogger.AppendErrorLogMessage('ReleaseBuffer Error, MapID is %d', [FBuffer.m_MapID]);
    {$ENDIF}

    if Result then
      FPoolDIBuffer.ReleaseBufferToPool(FBuffer);
  end;
end;

procedure TDIUdpServerEngine.FreeAllBuffers;
begin
  // 释放内存池中Buffer
  {$IFDEF _ICOP_DEBUG}
      _GlobalLogger.AppendLogMessage('FPoolDIBuffer.FreeBuffers管理释放内存, 个数: %d.',
                                   [FPoolDIBuffer.GetBufferCount]);
  {$ENDIF}
  FPoolDIBuffer.FreeBuffers;
end;  

procedure TDIUdpServerEngine.FreeAllPhysicsMemory;
begin
  FreeAllBuffers;
end;

procedure TDIUdpServerEngine.ProcessIOError(FIOEventType: IO_POST_EVENT_TYPE; FDIClientContext: TDIClientContext);
begin
end;

procedure TDIUdpServerEngine.ProcessIOMessage(FIOEventType: IO_POST_EVENT_TYPE; FDIClientContext: TDIClientContext; FBuffer: TDIBuffer; dwIoSize: DWORD);
begin
  case FIOEventType of
	  IO_WSA_RECV: OnWSARecv(FDIClientContext, FBuffer, dwIoSize);
	  IO_WSA_SEND: OnWSASend(FDIClientContext, FBuffer, dwIoSize);
  end;
end;

function TDIUdpServerEngine.PostWSARecvEvent(FDIClientContext: TDIClientContext; FRecvBuffer: TDIBuffer): Boolean;
var
  dwIOStatus: IO_RECV_STATUS;
begin
  Result := FALSE;


  if Assigned(FRecvBuffer) and (not m_bEngineShutDown) then begin

    // 投递一个RECV I/O Buffer
    dwIOStatus := PostWSARecv(FRecvBuffer.m_socket, FRecvBuffer, PROTOOOL_UDP);
    {$IFDEF _ICOP_DEBUGERR}
            case dwIOStatus of
              IO_RECV_WOULDBLOCK:
                _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_RECV_WOULDBLOCK, [0, 0]);
              IO_RECV_CLOSED:
                _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_RECV_CLOSED, [0, 0]);
              IO_RECV_RESET:
                _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_RECV_RESET, [0, 0]);
              IO_RECV_ERROR:
                _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_RECV_ERROR, [0, 0]);
            end;
    {$ENDIF}

    if ( (dwIOStatus <> IO_RECV_SUCCESS) and
         (dwIOStatus <> IO_RECV_IO_PENDING) and
         (dwIOStatus <> IO_RECV_WOULDBLOCK) ) then begin
      // 回收RECV Buffer
      ReleaseBuffer(FRecvBuffer);
    end
    else
    begin
      Result := TRUE;

      // 性能分析器 增加完成端口中Recv I/O数量
      {$IFDEF _IOCP_MONITOR}
          _IOCPMonitor.AddIOCPRecv;
      {$ENDIF}
    end;
  end;
end;

function TDIUdpServerEngine.OnWSARecv(FDIClientContext: TDIClientContext; FRecvBuffer: TDIBuffer; dwIoSize: DWORD): Boolean;
var
  FDIBuffer: TDIBuffer;
begin
  if Assigned(FRecvBuffer) then begin
    FRecvBuffer.SetUsed(dwIoSize);
    if Assigned(OnRecvCompletedEvent) then
      OnRecvCompletedEvent(FDIClientContext, FRecvBuffer, dwIoSize);
  end;

  // 投递下一个
  FDIBuffer := AllocateBuffer;
  FDIBuffer.m_Socket := m_sListenSocket;
  if FDIBuffer<>nil then PostWSARecvEvent(FDIClientContext, FDIBuffer);
  Result := TRUE;
end;


function TDIUdpServerEngine.PostWSASendEvent(FDIClientContext: TDIClientContext; FSendBuffer: TDIBuffer): Boolean;
var
  dwIOStatus: IO_SEND_STATUS;
begin
  Result := FALSE;
  
  if Assigned(FSendBuffer) and (not m_bEngineShutDown) then begin

    // 投递一个SEND I/O Buffer
    dwIOStatus := PostWSASend(FSendBuffer.m_Socket, FSendBuffer, PROTOOOL_UDP);
    {$IFDEF _ICOP_DEBUGERR}
          case dwIOStatus of
            IO_SEND_WOULDBLOCK:
              _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_SEND_WOULDBLOCK, [0, 0]);
            IO_SEND_RESET:
              _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_SEND_RESET, [0, 0]);
            IO_SEND_ERROR:
              _GlobalLogger.AppendErrorLogMessage(IOCP_ERROR_IO_SEND_ERROR, [0, 0]);
          end;
    {$ENDIF}

    if ( (dwIOStatus <> IO_SEND_SUCCESS) and
        (dwIOStatus <> IO_SEND_IO_PENDING) and
        (dwIOStatus <> IO_SEND_WOULDBLOCK) ) then begin

      // 回收SEND Buffer
      ReleaseBuffer(FSendBuffer);
    end
    else
    begin
      Result := TRUE;

      // 性能分析器 增加完成端口中Send I/O数量
      {$IFDEF _IOCP_MONITOR}
          _IOCPMonitor.AddIOCPSend;
      {$ENDIF}
    end;
  end;
end;

function TDIUdpServerEngine.OnWSASend(FDIClientContext: TDIClientContext; FSendBuffer: TDIBuffer; dwIoSize: DWORD): Boolean;
begin
  if Assigned(FSendBuffer) then begin
    FSendBuffer.SetUsed(dwIoSize);
    if Assigned(OnSendCompletedEvent) then
      OnSendCompletedEvent(FDIClientContext, FSendBuffer, dwIoSize);
  end;

  // 回收SEND Buffer
  ReleaseBuffer(FSendBuffer);
  Result := TRUE;
end;

function TDIUdpServerEngine.StartServer( nPort: Integer;
                                         iMaxNumConnections: DWORD;
                                         iMaxNumberOfFreeContext: DWORD;
                                         iMaxNumberOfFreeBuffer: DWORD;
                                         StartEngine: TENGINE_START_TYPE ): Boolean;
var
  bRet: LongBool;
  I: Integer;
  FFreeBuffer: TDIBuffer;
begin
  if (m_bEngineStarted) then begin
    Result := False;
    Exit;
  end;

  m_EngineType := StartEngine;
  _GlobalLogger.AppendDisplayMsg(IOCP_ENGINE_START, [m_sEngineName]);

  // 端口号
  m_nPort := nPort;
  // 设置m_bShutDown, 设置状态
  m_bEngineShutDown := FALSE;
  m_iNumberOfActiveConnections := 0;	                    // 当前客户端连接个数置零
  m_iMaxNumConnections := iMaxNumConnections;             // 允许最多客户端连接个数
  m_iMaxNumberOfFreeContext := iMaxNumberOfFreeContext;   // 空闲上下文池允许的最大数
  m_iMaxNumberOfFreeBuffer := iMaxNumberOfFreeBuffer;     // 空闲Buufer池允许的最大数
  m_iInitPostRecvBuffer := 100;                           // 保持投递的Recv数量

  // 初始化HasMap数量
  if Assigned(OnCreateClientContextEvent) then begin
    FMapClientContext.OnCreateClientContextEvent := OnCreateClientContextEvent;
    FMapClientContext.InitHasTableLength(m_iMaxNumConnections);
  end
  else
  begin
    _GlobalLogger.AppendDisplayMsg('OnCreateClientContextEvent is NULL.', []);
    Result := False;
    Exit;
  end;

  // 空闲Buffer池
  FPoolDIBuffer.SetMaxFreeBuffer(m_iMaxNumberOfFreeBuffer);
  for I:=1 to m_iMaxNumberOfFreeBuffer do begin
    FFreeBuffer := TDIBuffer.Create;
    FPoolDIBuffer.ReleaseBufferToPool(FFreeBuffer);
  end;


  // 启动IOCP性能监视器
  {$IFDEF _IOCP_MONITOR}
      _IOCPMonitor.StartMonitor;
     _GlobalLogger.AppendLogMessage('%s',['IOCP性能监视器启动成功.']);
  {$ENDIF}

  bRet := FALSE;
  if m_EngineType = SERVER_ENGINE_START then begin
    // 服务器端启动
    bRet := ListnerStart();
    if(bRet) then
      _GlobalLogger.AppendDisplayMsg('%s',['服务器端启动套接字侦听.'])
    else
    begin
      {$IFDEF _ICOP_DEBUG}
          _GlobalLogger.AppendLogMessage('%s',['系统异常错误，服务器端侦听线程启动失败.']);
      {$ENDIF}
      Result := FALSE;
      Exit;
    end;
  end;

  if m_EngineType = SERVER_ENGINE_START then begin
    if (bRet) then begin
      m_bEngineStarted := TRUE;
      _GlobalLogger.AppendDisplayMsg('IOCP服务器端侦听地址: %s, 端口号:%d.', [FWinSocket.GetHostIPAddr(), m_nPort]);
      _GlobalLogger.AppendDisplayMsg('%s',['IOCP服务器端启动成功.']);
    end;
  end
  else            
  begin
    m_bEngineStarted := TRUE;
    m_bAcceptConnections := TRUE;
    _GlobalLogger.AppendDisplayMsg('%s',['IOCP客户端启动成功.']);
  end;

  Result := TRUE;
end;


procedure TDIUdpServerEngine.StopServer;
begin
  if (m_bEngineStarted) then begin
    // 是否允许客户端连接
		m_bAcceptConnections := FALSE;
    {$IFDEF _ICOP_DEBUG}
        _GlobalLogger.AppendLogMessage('%s',['设置连接状态m_bAcceptConnections := FALSE.']);
    {$ENDIF}

    // 关闭所有连接
    {$IFDEF _ICOP_DEBUG}
        _GlobalLogger.AppendLogMessage('%s',['关闭所有客户端DisconnectAllClient成功.']);
    {$ENDIF}

    Sleep(0);

    // 设置关闭
    m_bEngineShutDown := TRUE;

    if m_EngineType = SERVER_ENGINE_START then begin
      // 停止侦听线程
      {$IFDEF _ICOP_DEBUG}
          _GlobalLogger.AppendLogMessage('%s',['WaitForSingleObject m_hListnerEngineThread.']);
      {$ENDIF}
      // SetEvent(m_hShutdownEvent);
      // Sleep(0);

      // WaitForSingleObject(m_hListnerEngineThread.Handle, INFINITE);
      // m_hListnerEngineThread.Free;
      // CloseHandle(m_hShutdownEvent);
      // m_hShutdownEvent := INVALID_HANDLE_VALUE;
      // CloseHandle(m_hListnerEngineThreadEvent);
      // CloseHandle(m_hPostAcceptEvent);
      // m_hPostAcceptEvent := INVALID_HANDLE_VALUE;

      // 关闭侦听套接字
      {$IFDEF _ICOP_DEBUG}
          _GlobalLogger.AppendLogMessage('%s',['关闭侦听套接字.']);
      {$ENDIF}
      WinSock2.shutdown(m_sListenSocket, SD_BOTH);
      FWinSocket.CloseWinSocket(m_sListenSocket);
      Sleep(0);
    end;

    // 关闭IOCP性能监视器
    {$IFDEF _IOCP_MONITOR}
        _IOCPMonitor.StopMonitor;
        _GlobalLogger.AppendLogMessage('%s',['IOCP性能监视器关闭成功.']);
    {$ENDIF}

    m_bEngineStarted := FALSE;
  end;

end;

end.
