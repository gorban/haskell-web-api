(() => {
  const pageLinkSelector = 'a[data-page-link="true"]';
  const navigationRegionSelector = 'nav[data-navigation-region="primary"]';
  const navigationContentSelector = 'main[data-navigation-content="true"]';
  let navigationInFlight = false;

  function isPlainLeftClick(event) {
    return event.button === 0 && !event.metaKey && !event.ctrlKey && !event.shiftKey && !event.altKey;
  }

  function toAbsoluteUrl(targetUrl) {
    return new URL(targetUrl, window.location.href);
  }

  function isSameOriginNavigationLink(anchor) {
    if (!anchor || anchor.target || anchor.hasAttribute('download')) {
      return false;
    }

    const absoluteUrl = toAbsoluteUrl(anchor.href);
    return absoluteUrl.origin === window.location.origin;
  }

  function syncBodyAttributes(nextBody) {
    const currentBody = document.body;
    const nextAttributes = new Map(Array.from(nextBody.attributes, (attribute) => [attribute.name, attribute.value]));

    Array.from(currentBody.attributes).forEach((attribute) => {
      if (!nextAttributes.has(attribute.name)) {
        currentBody.removeAttribute(attribute.name);
      }
    });

    nextAttributes.forEach((value, name) => {
      currentBody.setAttribute(name, value);
    });
  }

  function applyFetchedDocument(responseText, targetUrl, shouldPushState) {
    const parsedDocument = new DOMParser().parseFromString(responseText, 'text/html');
    const nextTitle = parsedDocument.querySelector('title');
    const nextNavigationRegion = parsedDocument.querySelector(navigationRegionSelector);
    const nextNavigationContent = parsedDocument.querySelector(navigationContentSelector);
    const currentNavigationRegion = document.querySelector(navigationRegionSelector);
    const currentNavigationContent = document.querySelector(navigationContentSelector);

    if (!nextTitle || !nextNavigationRegion || !nextNavigationContent || !currentNavigationRegion || !currentNavigationContent) {
      return false;
    }

    document.title = nextTitle.textContent || document.title;
    currentNavigationRegion.replaceWith(nextNavigationRegion);
    currentNavigationContent.replaceWith(nextNavigationContent);
    syncBodyAttributes(parsedDocument.body);

    if (shouldPushState) {
      window.history.pushState({ path: targetUrl }, '', targetUrl);
    }

    return true;
  }

  async function navigateTo(targetUrl, shouldPushState) {
    if (navigationInFlight) {
      return;
    }

    navigationInFlight = true;

    try {
      const response = await window.fetch(targetUrl, {
        credentials: 'same-origin',
        headers: {
          'X-Requested-With': 'tiny-navigation',
        },
      });

      if (!response.ok) {
        window.location.assign(targetUrl);
        return;
      }

      const responseText = await response.text();
      if (!applyFetchedDocument(responseText, targetUrl, shouldPushState)) {
        window.location.assign(targetUrl);
      }
    } catch (_error) {
      window.location.assign(targetUrl);
    } finally {
      navigationInFlight = false;
    }
  }

  function handleDocumentClick(event) {
    if (event.defaultPrevented || !isPlainLeftClick(event)) {
      return;
    }

    const anchor = event.target.closest(pageLinkSelector);
    if (!isSameOriginNavigationLink(anchor)) {
      return;
    }

    event.preventDefault();
    void navigateTo(anchor.href, true);
  }

  function handlePopState() {
    void navigateTo(window.location.href, false);
  }

  document.addEventListener('click', handleDocumentClick);
  window.addEventListener('popstate', handlePopState);
})();
