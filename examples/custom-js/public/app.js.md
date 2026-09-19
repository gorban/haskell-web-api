# public/app.js

```js
document.addEventListener("DOMContentLoaded", () => {
  const toggles = document.querySelectorAll("[data-disclosure-toggle]");

  for (const toggle of toggles) {
    toggle.addEventListener("click", () => {
      const target = document.getElementById(toggle.dataset.disclosureToggle);
      if (target) target.hidden = !target.hidden;
    });
  }
});
```

Keep custom JavaScript tiny, page-scoped when possible, and compatible with direct SSR loads.
