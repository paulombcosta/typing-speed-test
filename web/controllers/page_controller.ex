defmodule TypingSpeedTest3.PageController do
  use TypingSpeedTest3.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
