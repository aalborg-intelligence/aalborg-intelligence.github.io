// trim-categories.js
(function () {
  // To cifre + separator (mellemrum, underscore, bindestreg)
  const PREFIX = /^\d{2}([ _-])/;

  // Hjælp: find første direkte tekstnode i et element
  function firstTextNode(el) {
    for (const node of el.childNodes) {
      if (node.nodeType === Node.TEXT_NODE && node.nodeValue && node.nodeValue.trim().length > 0) {
        return node;
      }
    }
    return null;
  }

  function trimPrefixOn(el) {
    // Hvis elementet har børn (fx ikon + tekst), så trim kun første tekstnode
    const tn = firstTextNode(el);
    if (tn) {
      const raw = tn.nodeValue;
      const trimmedStart = raw.replace(/^\s+/, ''); // bevar leading spaces-ændring lokalt
      if (PREFIX.test(trimmedStart) && trimmedStart.length > 3) {
        // Bevar evt. oprindeligt indrykningsmellemrum før første ikke-whitespace
        const leading = raw.match(/^\s*/)?.[0] ?? '';
        tn.nodeValue = leading + trimmedStart.slice(3);
      }
      return;
    }

    // Fallback: hvis der ikke findes en tekstnode, så brug textContent (typisk <a> med ren tekst)
    const t = (el.textContent || '').trimStart();
    if (PREFIX.test(t) && t.length > 3) {
      // Brug IKKE på containere! (Vi kalder ikke funktionen på containere)
      el.textContent = t.slice(3);
    }
  }

  function run() {
    // VIGTIGT: Vi vælger kun chips, ikke containerne.
    const selector = [
      '.listing-category',                // chip i listings
      '.quarto-category',                 // chip i post-header
      '.quarto-categories .quarto-category',
      '.category:not(.categories)'        // undgå .categories container
    ].join(', ');

    document.querySelectorAll(selector).forEach(trimPrefixOn);
  }

  document.addEventListener('DOMContentLoaded', run);

  // Fanger chips der tilføjes dynamisk (fx ved client-side rendering)
  const mo = new MutationObserver(muts => {
    for (const m of muts) {
      if (m.type !== 'childList') continue;
      for (const node of m.addedNodes) {
        if (!(node instanceof Element)) continue;
        // Træk chips i noden
        const inner = node.querySelectorAll?.('.listing-category, .quarto-category, .quarto-categories .quarto-category, .category:not(.categories)');
        inner?.forEach(trimPrefixOn);
        // Hvis noden selv er en chip
        if (node.matches?.('.listing-category, .quarto-category, .quarto-categories .quarto-category, .category:not(.categories)')) {
          trimPrefixOn(node);
        }
      }
    }
  });

  document.addEventListener('DOMContentLoaded', () => {
    mo.observe(document.body, { childList: true, subtree: true });
  });
})();