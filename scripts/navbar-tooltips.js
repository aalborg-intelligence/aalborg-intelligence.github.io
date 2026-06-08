document.addEventListener('DOMContentLoaded', () => {
  // 1) Envelope (About) – find <a> som indeholder kuvert-ikonet
  const envLink = document.querySelector('.navbar a.nav-link .bi.bi-envelope')?.closest('a.nav-link');
  if (envLink) {
    envLink.setAttribute('title', 'Om os');
    envLink.setAttribute('aria-label', 'Om os');
  }

  // 2) YouTube – find <a> som indeholder youtube-ikonet ELLER href indeholder 'youtube'
  const ytLink =
    document.querySelector('.navbar a.nav-link .bi.bi-youtube')?.closest('a.nav-link') ||
    document.querySelector('.navbar a.nav-link[href*="youtube"]');
  if (ytLink) {
    ytLink.setAttribute('title', 'AI mat - YouTube');
    ytLink.setAttribute('aria-label', 'AI mat - YouTube');
  }

  // 3) Search – knappen har klassen .quarto-search-button
  const searchBtn = document.querySelector('.quarto-search-button');
  if (searchBtn) {
    searchBtn.setAttribute('title', 'Søg');
    searchBtn.setAttribute('aria-label', 'Søg');
  }
});


document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll(".collapsible").forEach(function (h) {
    const content = h.nextElementSibling;

    if (content && content.classList.contains("collapse-content")) {
      
      // sørg for korrekt initial state
      content.style.display = "none";
      h.classList.remove("open");

      h.addEventListener("click", function () {
        const isOpen = content.style.display === "block";

        content.style.display = isOpen ? "none" : "block";
        h.classList.toggle("open");
      });
    }
  });
});
