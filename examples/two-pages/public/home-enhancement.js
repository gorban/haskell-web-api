export function setupPageEnhancement(root) {
  const status = root.querySelector("[data-home-enhancement-status]");
  if (!status) return undefined;

  status.dataset.homeEnhancementReady = "true";
  status.textContent = "The page-scoped home enhancement is ready.";
  return () => {
    status.removeAttribute("data-home-enhancement-ready");
  };
}
