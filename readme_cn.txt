设置环境变量
DIOCP3_HOME=E:\workspace\diocp3

工程搜索路径
$(DIOCP3_HOME)\Source\iocp-engine;$(DIOCP3_HOME)\Source\iocp-socket;$(DIOCP3_HOME)\Source\utils;$(DIOCP3_HOME)\Source\iocp-task


重写了DIOCP，版本从3开始。

新特性:
  1.重新规划了整个设计。抽象IOCP层->IOCP-Socket(Socket应用),将iocp独立出来，可以做更多的iocp应用。
  2.改用投递Request->Response的模式，这样每个投递的请求，都可以处理响应。(感谢ryan提供的思路)
  3.去掉了IO内存池，服务端可以0拷贝处理和转移数据。
  4.去掉了Accept线程，改用AcceptEx,响应连接更好。
  5.添加了TIocpTcpClient客户端组件，iocp客户端的应用，可以很好的做推送消息客户端。
  6.组件化TIocpTcpClient和TIocpTcpServer。

  7.添加了基于IOCP引擎的任务投递(异步+回调)库iocp-task
  8.封装了DIOCP1函数接口类，同样支持解码和编码器。
