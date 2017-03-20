defmodule Triptych.Crud do 

  use GenServer
  
  @process_name :triptych_memo
  
  #
  # low-level 
  #
  def init(stash_pid) do
    current = Triptych.Stash.get_value stash_pid
    { :ok, { current, stash_pid }}
  end
  
  def handle_cast({ :add, triple = { subject, predicate, object } }, { { sub, pre, obj }, stash_pid }) do
    { :noreply, { { add_helper(subject, triple, sub), add_helper(predicate, triple, pre), add_helper(object, triple, obj) }, stash_pid } }
  end 
  
  def handle_cast({ :delete, { subject, predicate, object } }, { { sub, pre, obj }, stash_pid }) do 
    { :noreply, { { delete_helper(subject, sub), delete_helper(predicate, pre), delete_helper(object, obj) }, stash_pid } }
  end 
  
  # find triples: there are many, many possible cases...
  def handle_call({ :find, triple }, _from, { dicts = { sub, pre, obj }, stash_pid } ) do
    found =
      case triple do
        { :blank, :blank, :blank }     ->
          Map.values(sub) |> Enum.reduce(MapSet.new, &MapSet.union/2)
        { :blank, :blank, object }     ->
          dict_get obj, object
        { :blank, predicate, :blank }  ->
          dict_get pre, predicate
        { subject, :blank, :blank }    ->
          dict_get sub, subject
        { subject, predicate, :blank }  ->
          MapSet.intersection dict_get(sub, subject), dict_get(pre, predicate)
        { :blank, predicate, object }  ->
          MapSet.intersection dict_get(obj, object), dict_get(pre, predicate)
        { subject, :blank, object }    ->
          MapSet.intersection dict_get(sub, subject), dict_get(obj, object)
        { subject, predicate, object } -> 	
          MapSet.intersection MapSet.new([{ subject, predicate, object }]), dict_get(sub, subject) 
      end
    { :reply, found, { dicts, stash_pid } }
  end
  
  def terminate(_reason, { current, stash_pid }) do
    Triptych.Stash.save_value stash_pid, current  
  end
  
  #  
  # API  
  #
  def start_link(stash_pid) do
    :gen_server.start_link({ :local, @process_name }, __MODULE__, stash_pid, [])
  end
  
  def add(triple) do
    if is_triple? triple do
      :gen_server.cast( @process_name, { :add, triple } )
    else
      IO.puts "Sorry, this is not a triple:"
      IO.inspect triple
    end
  end
  
  def find(triple) do
    :gen_server.call( @process_name, { :find, triple } ) |> MapSet.to_list
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
    if(Map.has_key?(dict, key)) do
      # Update list
      Map.update! dict, key, fn(set) -> MapSet.union(set, MapSet.new([ value ])) end
    else
      # Create list, add value
      Map.put dict, key, MapSet.new([ value ])
    end
  end
  
  def delete_helper(_key, dict) do
    # ToDo
    dict
  end
  
  def dict_get(dict, key) do
    Map.get( dict, key, MapSet.new ) 
  end
  
  def is_triple?({ _fst, _scd, _thr }) do
    true
  end
  
  def is_triple?(_err_value) do
    false
  end

end

