unit uIOCPCentre;

interface

uses
  iocpTcpServer, uBuffer, SysUtils;

type
  TIOCPDecoder = class(TObject)
  public
    /// <summary>
    ///   解码收到的数据,如果有接收到数据,调用该方法,进行解码
    /// </summary>
    /// <returns>
    ///   返回解码好的对象
    /// </returns>
    /// <param name="inBuf"> 接收到的流数据 </param>
    function Decode(const inBuf: TBufferLink): TObject; virtual; abstract;
  end;

  TIOCPDecoderClass = class of TIOCPDecoder;

  TIOCPEncoder = class(TObject)
  public
    /// <summary>
    ///   编码要发送的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="ouBuf"> 编码好的数据 </param>
    procedure Encode(pvDataObject:TObject; const ouBuf: TBufferLink); virtual;
        abstract;
  end;

  TIOCPEncoderClass = class of TIOCPEncoder;

  TIOCPClientContext = class(iocpTcpServer.TiocpClientContext)
  private
    FrecvBuffers: TBufferLink;
    FStateINfo: String;
    function GetStateINfo: String;
  protected
    procedure add2Buffer(buf:PAnsiChar; len:Cardinal);
    procedure clearRecvedBuffer;
    function decodeObject: TObject;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    procedure recvBuffer(buf:PAnsiChar; len:Cardinal); virtual;
  protected
  public
    constructor Create;override;
    destructor Destroy; override;
    /// <summary>
    ///   数据处理
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure dataReceived(const pvDataObject:TObject); virtual;
    /// <summary>
    ///   将数据返回给客户端
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure writeObject(const pvDataObject:TObject);

    /// <summary>
    ///   接受的Buffer
    /// </summary>
    property Buffers: TBufferLink read FrecvBuffers;
    //状态信息
    property StateINfo: String read GetStateINfo write FStateINfo;
  end;


  TIOCPConsole = class(TIocpTcpServer)
  private
    FInnerEncoder:TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;

    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
  public
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure registerCoderClass(pvDecoderClass:TIOCPDecoderClass;
        pvEncoderClass:TIOCPEncoderClass);
    /// <summary>
    ///   注册解码器
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure registerDecoder(pvDecoder:TIOCPDecoder);

    /// <summary>
    ///   注册编码器
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure registerEncoder(pvEncoder:TIOCPEncoder);
  end;



implementation

uses
  uIOCPFileLogger;

constructor TIOCPClientContext.Create;
begin
  inherited Create;
  FrecvBuffers := TBufferLink.Create();
end;

destructor TIOCPClientContext.Destroy;
begin
  FrecvBuffers.Free;
  inherited Destroy;
end;

procedure TIOCPClientContext.add2Buffer(buf: PAnsiChar; len: Cardinal);
begin
  //加入到套接字对应的缓存
  FrecvBuffers.AddBuffer(buf, len);
end;

procedure TIOCPClientContext.clearRecvedBuffer;
begin
  if FrecvBuffers.validCount = 0 then
  begin
    FrecvBuffers.clearBuffer;
  end else
  begin
    FrecvBuffers.clearHaveReadBuffer;
  end;
end;

procedure TIOCPClientContext.dataReceived(const pvDataObject:TObject);
begin

end;

function TIOCPClientContext.decodeObject: TObject;
begin
  Result := TIocpConsole(Owner).FDecoder.Decode(FrecvBuffers);
end;

function TIOCPClientContext.GetStateINfo: String;
begin
  Result := FStateINfo;
end;

procedure TIOCPClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: WORD);
begin
  recvBuffer(buf, len);
end;

procedure TIOCPClientContext.recvBuffer(buf:PAnsiChar; len:Cardinal);
var
  lvObject:TObject;
begin
  add2Buffer(buf, len);

  self.StateINfo := '接收到数据,准备进行解码';

  ////避免一次收到多个包时导致只调用了一次逻辑的处理(dataReceived);
  ///  2013年9月26日 08:57:20
  ///    感谢群内JOE找到bug。
  while True do
  begin
    //调用注册的解码器<进行解码>
    lvObject := decodeObject;
    if Integer(lvObject) = -1 then
    begin
      /// 错误的包格式, 关闭连接
      Disconnect;
      exit;
    end else if lvObject <> nil then
    begin
      try
        try
          self.StateINfo := '解码成功,准备调用dataReceived进行逻辑处理';

          //解码成功，调用业务逻辑的处理方法
          dataReceived(lvObject);

          self.StateINfo := 'dataReceived逻辑处理完成!';
        except
          on E:Exception do
          begin
            TIOCPFileLogger.logErrMessage('截获处理逻辑异常!' + e.Message);
          end;
        end;
      finally
        lvObject.Free;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;

  //清理缓存<如果没有可用的内存块>清理
  clearRecvedBuffer;
end;

procedure TIOCPClientContext.writeObject(const pvDataObject:TObject);
var
  lvOutBuffer:TBufferLink;
begin
//
//  if FSendCache.Count > 10 then
//  begin
//    TIOCPFileLogger.logMessage('TIOCPClientContext.writeObject: 待发送的缓存队列超过10个, 可能客户端恶意不进行接收, 踢掉连接!', 'DIOCP_TRACE_');
//    self.closeClientSocket;
//    exit;
//  end;
//
//  //解码
//  lvOutBuffer := TBufferLink.Create;
//  try
//    self.StateINfo := 'TIOCPClientContext.writeObject,准备编码对象到lvOutBuffer';
//    TIOCPContextFactory.instance.FEncoder.Encode(pvDataObject, lvOutBuffer);
//  except
//    lvOutBuffer.Free;
//    raise;
//  end;
//
//  FSendCacheLocker.Enter;
//  try
//    //添加到待发送的列表
//    FSendCache.Add(lvOutBuffer);
//
//    if FCurrentSendBuffer = nil then
//    begin
//      FCurrentSendBuffer := lvOutBuffer;
//
//      //准备投递一块数据
//      checkPostWSASendCache;
//    end;
//    //不为nil说明还有需要投递的任务
//
//  finally
//    FSendCacheLocker.Leave;
//  end;

  self.StateINfo := 'TIOCPClientContext.writeObject,投递到发送缓存';

end;

procedure TIOCPConsole.registerCoderClass(pvDecoderClass: TIOCPDecoderClass;
    pvEncoderClass: TIOCPEncoderClass);
begin
  if FInnerDecoder <> nil then
  begin
    raise Exception.Create('已经注册了解码器类');
  end;

  FInnerDecoder := pvDecoderClass.Create;
  registerDecoder(FInnerDecoder);

  if FInnerEncoder <> nil then
  begin
    raise Exception.Create('已经注册了编码器类');
  end;
  FInnerEncoder := pvEncoderClass.Create;
  registerEncoder(FInnerEncoder);
end;

{ TIOCPConsole }

procedure TIOCPConsole.registerDecoder(pvDecoder: TIOCPDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TIOCPConsole.registerEncoder(pvEncoder: TIOCPEncoder);
begin
  FEncoder := pvEncoder;
end;

end.
