
defmodule Triptych.Crud do 

  use GenServer.Behaviour
  
  @process_name :triptych_memo
  
  #
  # low-level 
  #
  def init(init_memo) do
    { :ok, { HashDict.new(init_memo), HashDict.new(init_memo), HashDict.new(init_memo) }}
  end
  
  def handle_cast({ :add, triple = { subject, predicate, object } }, { sub, pre, obj }) do
    { :noreply, { add_helper(subject, triple, sub), add_helper(predicate, triple, pre), add_helper(object, triple, obj) } }
  end  
  
  # find triples: there are many, many possible cases...
  def handle_call({ :find, { subject, predicate, object } }, dicts = { sub, pre, obj }) do
    found = HashDict.get(sub, subject, []) ++ HashDict.get(pre, predicate, []) ++ HashDict.get(obj, object, []) |> Enum.uniq
    { :reply, found, dicts }
  end
  
  def handle_call({ :find, { subject, predicate, :blank } }, dicts = { sub, pre, _obj }) do
    found = HashDict.get(sub, subject, []) ++ HashDict.get(pre, predicate, []) |> Enum.uniq
    { :reply, found, dicts }
  end
  
  def handle_call({ :find, { subject, :blank, :blank } }, dicts = { sub, _pre, _obj }) do
    found = HashDict.get(sub, subject, [])
    { :reply, found, dicts }
  end
  
  def handle_call({ :find, { :blank, predicate, object } }, dicts = { _sub, pre, obj }) do
    found = HashDict.get(pre, predicate, []) ++ HashDict.get(obj, object, []) |> Enum.uniq
    { :reply, found, dicts }
  end
  
  def handle_call({ :find, { :blank, :blank, object } }, dicts = { _sub, _pre, obj }) do
    found = HashDict.get(obj, object, [])
    { :reply, found, dicts }
  end
  
  def handle_call({ :find, { subject, :blank, object } }, dicts = { sub, _pre, obj }) do
    found = HashDict.get(sub, subject, []) ++ HashDict.get(obj, object, []) |> Enum.uniq
    { :reply, found, dicts }
  end
  
  def handle_call({ :find, { :blank, predicate, :blank } }, dicts = { _sub, pre, _obj }) do
    found = HashDict.get(pre, predicate, [])
    { :reply, found, dicts }
  end
  
  # get all
  def handle_call({ :find, { :blank, :blank, :blank } }, dicts = { sub, _pre, _obj }) do
    found = HashDict.values(sub) |> Enum.reduce [], fn(ele, acc) -> ele ++ acc end 
    { :reply, found, dicts }
  end
  
  #  
  # API  
  #
  def start_link(init_memo) do
    :gen_server.start_link({ :local, @process_name }, __MODULE__, init_memo, [])
  end
  
  def add(triple) do
    :gen_server.cast( @process_name, { :add, triple } )
  end
  
  def find(triple) do
    :gen_server.call( @process_name, { :find, triple } )
  end
  
  # def update do
  # 
  # end
  
  def delete(triple) do
    :gen_server.call( @process_name, { :delete, triple } )
  end
  
  def all() do
    find({ :blank, :blank, :blank }) 
  end


  #
  # Helper
  #
  def add_helper(key, value, dict) do
    if(HashDict.has_key?(dict, key)) do
      # Update list
      l = HashDict.get dict, key
      HashDict.update dict, key, [ value | l ]
    else
      # Create list, add value
      HashDict.put dict, key, [ value ]
    end
  end

end

