const sortables = document.querySelectorAll("[js-todos]");

for(e of sortables) {
  new Sortable(e, {
    group: 'todo',
    animation: 150,

    onEnd: (ev) => {
      const reordered = new Event("reordered");
      reordered.from = ev.from.dataset["groupid"];
      reordered.to = ev.to.dataset["groupid"];
      reordered.oldord = ev.oldIndex;
      reordered.neword = ev.newIndex;
      console.dir(reordered);
      ev.to.dispatchEvent(reordered);
    },
  })
}
