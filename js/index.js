const sortables = document.querySelectorAll("[js-todos]");

for(e of sortables) {
  new Sortable(e, {
    group: 'todo',
    animation: 150,
  })
}
