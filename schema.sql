PRAGMA recursive_triggers = 1; 
create trigger update_todo_order before update of "order" on todo
begin
  update todo set "order" = "order" + 1
         where "order" = new."order" and "group" = new."group";
end;
create trigger insert_todo_order before insert on todo
begin
  update todo set "order" = "order" + 1
         where "order" = new."order" and "group" = new."group";
end;

insert into todo ("order", desc, done_p, "group") values (0, 'Another test', 1, 1);
