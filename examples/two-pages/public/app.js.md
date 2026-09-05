# public/app.js

```js
document.addEventListener("click", async (event) => {
  const link = event.target.closest("a[data-page-link='true']");
  if (!link) return;
  if (link.origin !== window.location.origin) return;

  event.preventDefault();
  const response = await fetch(link.href, {
    headers: { "X-Requested-With": "page-link" }
  });

  if (!response.ok) {
    window.location.assign(link.href);
    return;
  }

  const html = await response.text();
  document.open();
  document.write(html);
  document.close();
  window.history.pushState({}, "", link.href);
});
```

The current repo already ships a tiny navigation runtime in `packages/web-api/public/navigation.js`.
This snippet exists to show the intended size and responsibility of the browser layer.
