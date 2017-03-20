
defmodule Triptych.Stash do

  use GenServer
  
  def start_link(current) do
    :gen_server.start_link( __MODULE__, current, [] )
  end
  
  
  # API
  def save_value(pid, value) do
    :gen_server.cast pid, { :save_value, value }
  end

  def get_value(pid) do
    :gen_server.call pid, :get_value
  end
  
  # low level
  def init(current) do
    { :ok, current }
  end
  
  def handle_call( :get_value, _from, current ) do
    { :reply, current, current }
  end
  
  def handle_cast( { :save_value, value }, _current ) do
    { :noreply, value }
  end
  
end
