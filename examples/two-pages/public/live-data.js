const liveRegion = document.querySelector("[data-live-data-source]");

if (liveRegion && "EventSource" in window) {
  const status = liveRegion.querySelector("[data-live-data-status]");
  const eventSource = new EventSource(liveRegion.dataset.liveDataSource);

  eventSource.addEventListener("update", (event) => {
    if (status) status.textContent = event.data;
    eventSource.close();
  });

  eventSource.addEventListener("error", () => eventSource.close());
}
