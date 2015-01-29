(*
 *	 Unit owner: D10.Mofen, delphi iocp framework author
 *         homePage: http://www.Diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *    Http协议处理单元
 *    其中大部分思路来自于delphi iocp framework中的iocp.HttpServer
 *
*)
unit DiocpHttpObject;

interface

uses
  Classes, StrUtils, SysUtils, uBuffer,
  iocpTcpServer;

type
  TDiocpHttpState = (hsCompleted, hsRequest);
  TDiocpHttpResponse = class;
  TDiocpHttpRequest = class(TObject)
  private
    FHeadMethod : string;
    FUrlPath: String;
    FRequestParams: String;
    FRawHttpData: TMemoryStream;

    FRequestHeader: TStringList;

    FResponse: TDiocpHttpResponse;


    /// <summary>
    ///   是否有效的Http头
    /// </summary>
    /// <returns>
    ///   0: 数据不足够进行解码
    ///   1: 有效的数据头
    ///   2: 无效的请求数据头
    /// </returns>
    function DecodeHeadRequest: Integer;

    /// <summary>
    ///   解码Http内容, 接受完整的Http数据后执行
    /// </summary>
    /// <returns>
    ///   1: 有效的Http数据
    ///   0: 有效的Http数据
    /// </returns>
    function DecodeHttpContext: Integer;

    /// <summary>
    ///   接收到的Buffer,写入数据
    /// </summary>
    procedure WriteRawBuffer(const Buffer: Pointer; len: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   清理
    /// </summary>
    procedure Clear;

    /// <summary>
    ///  Http响应对象，回写数据
    /// </summary>
    property Response: TDiocpHttpResponse read FResponse;
  end;

  TDiocpHttpResponse = class(TObject)
  private
    FData: TMemoryStream;    
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Http 客户端连接
  /// </summary>
  TDiocpHttpClientContext = class(TIocpClientContext)
  private
    FHttpState: TDiocpHttpState;
    FRequest: TDiocpHttpRequest;
  public
    constructor Create; override;
    destructor Destroy; override;
  protected
    /// <summary>
    ///   归还到对象池，进行清理工作
    /// </summary>
    procedure DoCleanUp; override;

    /// <summary>
    ///   接收到客户端的Http协议数据, 进行解码成TDiocpHttpRequest，响应Http请求
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
  end;

  /// <summary>
  ///  响应请求
  /// </summary>
  TOnDiocpHttpRequest = procedure(pvRequest:TDiocpHttpRequest) of object;

  /// <summary>
  ///   Http 解析服务
  /// </summary>
  TDiocpHttpServer = class(TIocpTcpServer)
  private
    FOnDiocpHttpRequest: TOnDiocpHttpRequest;

    /// <summary>
    ///   响应Http请求， 执行响应事件
    /// </summary>
    procedure DoRequest(pvRequest:TDiocpHttpRequest);
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   响应Http请求事件
    /// </summary>
    property OnDiocpHttpRequest: TOnDiocpHttpRequest read FOnDiocpHttpRequest write
        FOnDiocpHttpRequest;
  end;

implementation

//delphi 最快速编码 URLDecode URLEncode

function URLDecode(const S: string): string;
var
  Idx: Integer;   // loops thru chars in string
  Hex: string;    // string of hex characters
  Code: Integer; // hex character code (-1 on error)
begin
  // Intialise result and string index
  Result := '';
  Idx := 1;
  // Loop thru string decoding each character
  while Idx <= Length(S) do
  begin
    case S[Idx] of
      '%':
      begin
        // % should be followed by two hex digits - exception otherwise
        if Idx <= Length(S) - 2 then
        begin
          // there are sufficient digits - try to decode hex digits
          Hex := S[Idx+1] + S[Idx+2];
          Code := SysUtils.StrToIntDef('$' + Hex, -1);
          Inc(Idx, 2);
        end
        else
          // insufficient digits - error
          Code := -1;
        // check for error and raise exception if found
        if Code = -1 then
          raise SysUtils.EConvertError.Create(
            'Invalid hex digit in URL'
          );
        // decoded OK - add character to result
        Result := Result + Chr(Code);
      end;
      '+':
        // + is decoded as a space
        Result := Result + ' '
      else
        // All other characters pass thru unchanged
        Result := Result + S[Idx];
    end;
    Inc(Idx);
  end;
end;


function URLEncode(const S: string; const InQueryString: Boolean): string;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    case S[Idx] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + S[Idx];
      ' ':
        if InQueryString then
          Result := Result + '+'
        else
          Result := Result + '%20';
      else
        Result := Result + '%' + SysUtils.IntToHex(Ord(S[Idx]), 2);
    end;
  end;
end;

procedure TDiocpHttpRequest.Clear;
begin
  FRawHttpData.Clear;
end;

constructor TDiocpHttpRequest.Create;
begin
  inherited Create;
  FRawHttpData := TMemoryStream.Create();
  FResponse := TDiocpHttpResponse.Create();
end;

destructor TDiocpHttpRequest.Destroy;
begin
  FreeAndNil(FResponse);
  FRawHttpData.Free;
  inherited Destroy;
end;

function TDiocpHttpRequest.DecodeHeadRequest: Integer;
var
  lvBuf:Pointer;
begin
  Result := 0;
  if FRawHttpData.Size <= 7 then Exit;

  lvBuf := FRawHttpData.Memory;

  if FHeadMethod <> '' then
  begin
    Result := 1;  // 已经解码
    Exit;
  end;


  Result := 1;
  // HTTP 1.1 支持8种请求
  if (StrLIComp(lvBuf, 'GET', 3) = 0) then
  begin
    FHeadMethod := 'GET';
  end else if (StrLIComp(lvBuf, 'POST', 4) = 0) then
  begin
    FHeadMethod := 'POST';
  end else if (StrLIComp(lvBuf, 'PUT', 3) = 0) then
  begin
    FHeadMethod := 'PUT';
  end else if (StrLIComp(lvBuf, 'HEAD', 3) = 0) then
  begin
    FHeadMethod := 'HEAD';
  end else if (StrLIComp(lvBuf, 'OPTIONS', 7) = 0) then
  begin
    FHeadMethod := 'OPTIONS';
  end else if (StrLIComp(lvBuf, 'DELETE', 6) = 0) then
  begin
    FHeadMethod := 'DELETE';
  end else if (StrLIComp(lvBuf, 'TRACE', 5) = 0) then
  begin
    FHeadMethod := 'TRACE';
  end else if (StrLIComp(lvBuf, 'CONNECT', 7) = 0) then
  begin
    FHeadMethod := 'CONNECT';
  end else
  begin
    Result := 2;
  end;
end;

function TDiocpHttpRequest.DecodeHttpContext: Integer;
var
  lvRawString: AnsiString;
  lvRequestCmdLine, lvMethod, lvTempStr, lvRawTemp:String;
  i, j:Integer;
begin
  Result := 1;
  SetLength(lvRawString, FRawHttpdata.Size);
  FRawHttpData.Read(lvRawString[1], FRawHttpdata.Size);
  FRequestHeader.Text := lvRawString;

  // GET /test?v=abc HTTP/1.1
  lvRequestCmdLine := FRequestHeader[0];
  FRequestHeader.Delete(0);

  I := 1;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求方法(GET, POST, PUT, HEAD...)
  lvMethod := UpperCase(Copy(lvRequestCmdLine, 1, I - 1));
  Inc(I);
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);

  // 请求参数及路径
  lvTempStr := Copy(lvRequestCmdLine, J, I - J);
  // 解析参数
  J := Pos('?', lvTempStr);

  if (J <= 0) then
  begin
    FUrlPath := lvTempStr;
    lvRawTemp := '';

    FUrlPath := URLDecode(FUrlPath);
    FRequestParams := '';
  end else
  begin
    FUrlPath := Copy(lvTempStr, 1, J - 1);
    lvRawTemp := Copy(lvTempStr, J + 1, MaxInt);

    FUrlPath := URLDecode(FUrlPath);
    FRequestParams := URLDecode(lvRawTemp);
  end;
end;

procedure TDiocpHttpRequest.WriteRawBuffer(const Buffer: Pointer; len: Integer);
begin
  FRawHttpData.WriteBuffer(Buffer^, len);
end;

constructor TDiocpHttpResponse.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create();
end;

destructor TDiocpHttpResponse.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

constructor TDiocpHttpClientContext.Create;
begin
  inherited Create;
  FRequest := TDiocpHttpRequest.Create();
end;

destructor TDiocpHttpClientContext.Destroy;
begin
  FreeAndNil(FRequest);
  inherited Destroy;
end;

procedure TDiocpHttpClientContext.DoCleanUp;
begin
  inherited;
  FHttpState := hsCompleted;
end;

procedure TDiocpHttpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    ErrCode: WORD);
var
  lvTmpBuf: PAnsiChar;
  CR, LF: Integer;
  lvRemain:Cardinal;
begin
  inherited;
  if FHttpState = hsCompleted then
  begin
    FRequest.Clear;
    FHttpState := hsRequest;
  end;

  lvTmpBuf := buf;
  CR := 0;
  LF := 0;
  lvRemain := len;
  while (lvRemain > 0) do
  begin
    if (FHttpState = hsRequest) then
    begin
      case lvTmpBuf^ of
        #13: Inc(CR);
        #10: Inc(LF);
      else
        CR := 0;
        LF := 0;
      end;

      // 写入请求数据
      FRequest.WriteRawBuffer(lvTmpBuf, 1);

      if FRequest.DecodeHeadRequest = 2 then
      begin    // 无效的Http请求
        self.RequestDisconnect('无效的Http请求', Self);
        Exit;
      end;

      // 请求数据已接收完毕(#13#10#13#10是HTTP请求结束的标志)
      if (CR = 2) and (LF = 2) then
      begin
        if FRequest.DecodeHttpContext = 0 then
        begin
          Self.RequestDisconnect('无效的Http协议数据', Self);
          Exit;
        end;

        // 响应事件
        TDiocpHttpServer(FOwner).DoRequest(FRequest);        

        // 改变Http状态
        FHttpState := hsCompleted;
      end;

      Dec(lvRemain);
      Inc(lvTmpBuf);
    end;

//    if (Client.FHttpState = hcPostData) then
//    begin
//      Inc(Client.FPostDataSize, len);
//      if Client.FAcceptPostData then
//        Client.FRequestPostData.Write(pch^, len);
//
//      if (Client.FPostDataSize >= Client.FRequestContentLength) then
//        Client.FHttpState := hcDone;
//
//      // Post数据直接剩余部分整段处理，到这里就已经全部处理完了，直接跳出循环
//      Break;
//    end;
  end;

//  // 在解析完请求数据之后再调用线程池
//  if (Client.FHttpState = hcDone) then
//  begin
//    {$ifdef __IOCP_HTTP_SERVER_LOGIC_THREAD_POOL__}
//    if (Client.AddRef = 1) then Exit;
//    FJobThreadPool.AddRequest(TIocpHttpRequest.Create(Client));
//    {$else}
//    DoOnRequest(Client);
//    {$endif}
//  end;





end;


{ TDiocpHttpServer }

constructor TDiocpHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  registerContextClass(TDiocpHttpClientContext);
end;

procedure TDiocpHttpServer.DoRequest(pvRequest: TDiocpHttpRequest);
begin
   if Assigned(FOnDiocpHttpRequest) then
   begin
     FOnDiocpHttpRequest(pvRequest);
   end;
end;

end.
