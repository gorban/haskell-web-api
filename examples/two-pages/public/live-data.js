export function setupPageEnhancement(root) {
  const liveRegion = root.querySelector("[data-live-data-source]");
  if (!liveRegion || !("EventSource" in window)) return undefined;

  const status = liveRegion.querySelector("[data-live-data-status]");
  const eventSource = new EventSource(liveRegion.dataset.liveDataSource);
  let released = false;

  const release = () => {
    if (!released) {
      released = true;
      eventSource.close();
    }
  };

  eventSource.addEventListener("update", (event) => {
    if (status) status.textContent = event.data;
    release();
  });

  eventSource.addEventListener("error", release);
  return release;
}
