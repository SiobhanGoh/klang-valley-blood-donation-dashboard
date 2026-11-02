// Flip, 3D tilt and ripple for the dashboard. Tilt and ripple are skipped for
// people who prefer reduced motion; tilt is also skipped on touch devices.
(function () {
  var reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var finePointer = window.matchMedia("(hover: hover) and (pointer: fine)").matches;
  var MAX_TILT = 7; // degrees

  function toggleFlip(card) {
    var flipped = card.classList.toggle("is-flipped");
    card.setAttribute("aria-pressed", flipped ? "true" : "false");
  }

  function addRipple(host, event) {
    if (reduceMotion) return;
    var rect = host.getBoundingClientRect();
    var size = Math.max(rect.width, rect.height);
    var x = (event.clientX || rect.left + rect.width / 2) - rect.left - size / 2;
    var y = (event.clientY || rect.top + rect.height / 2) - rect.top - size / 2;
    var dot = document.createElement("span");
    dot.className = "ripple";
    dot.style.width = dot.style.height = size + "px";
    dot.style.left = x + "px";
    dot.style.top = y + "px";
    host.appendChild(dot);
    dot.addEventListener("animationend", function () { dot.remove(); });
  }

  // Event delegation so it works for content Shiny renders later.
  document.addEventListener("click", function (e) {
    var card = e.target.closest(".flip-card");
    if (card) {
      toggleFlip(card);
      var face = card.classList.contains("is-flipped")
        ? card.querySelector(".flip-back") : card.querySelector(".flip-front");
      if (face) addRipple(face, e);
      return;
    }
    var host = e.target.closest(".navbar .nav-link, .ripple-host");
    if (host) {
      host.classList.add("ripple-host");
      addRipple(host, e);
    }
  });

  document.addEventListener("keydown", function (e) {
    var card = e.target.closest && e.target.closest(".flip-card");
    if (card && (e.key === "Enter" || e.key === " ")) {
      e.preventDefault();
      toggleFlip(card);
    }
  });

  if (!reduceMotion && finePointer) {
    document.addEventListener("pointermove", function (e) {
      var card = e.target.closest && e.target.closest(".flip-card");
      if (!card) return;
      var tilt = card.querySelector(".flip-tilt");
      var rect = card.getBoundingClientRect();
      var px = (e.clientX - rect.left) / rect.width;   // 0..1
      var py = (e.clientY - rect.top) / rect.height;   // 0..1
      tilt.style.transform =
        "rotateX(" + ((0.5 - py) * 2 * MAX_TILT).toFixed(2) + "deg) " +
        "rotateY(" + ((px - 0.5) * 2 * MAX_TILT).toFixed(2) + "deg) translateZ(0)";
      card.style.setProperty("--gx", (px * 100).toFixed(1) + "%");
      card.style.setProperty("--gy", (py * 100).toFixed(1) + "%");
    });
    document.addEventListener("pointerout", function (e) {
      var card = e.target.closest && e.target.closest(".flip-card");
      if (card && !card.contains(e.relatedTarget)) {
        card.querySelector(".flip-tilt").style.transform = "";
      }
    });
  }
})();
