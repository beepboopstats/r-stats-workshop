// attendance-lookup.js -- participant self-check for content/attendance.qmd
//
// Reads ../assets/attendance/lookup.json, which holds ONLY ciphertext. Each
// participant's record is AES-GCM encrypted under a key derived from their own
// email address, so a record can only be opened by someone who types that
// address. The file is generated privately (dev/zoom-attendance/encrypt-lookup.mjs);
// the scheme below must stay in step with that script.
//
// Corrections and the Module 4 RSVP go to a Google Form, opened pre-filled.
// Until FORM.url is set, the page falls back to a pre-written email instead.
(function () {
  "use strict";

  // --- Google Form config -------------------------------------------------
  // Paste the form's base URL (ending in /viewform) and the entry.NNN id of
  // each question, taken from the form's "Get pre-filled link".
  const FORM = {
    url: "https://docs.google.com/forms/d/e/1FAIpQLSdOzgrAnWlPHOtWeDZsCNG_t4MXZDm_iW3LNsf18Y6aOPJqHw/viewform",
    fields: {
      email: "entry.942880674",          // email looked up with
      name: "entry.1185417601",          // name for the certificate
      otherEmails: "entry.383306557",    // other emails used
      modules: "entry.1488421833",       // checkboxes: "Module 1" / "Module 2" / "Module 3"
      // notes and the Module 4 RSVP are left blank for the participant to answer
    },
    // label of each checkbox option in the form, by module number
    moduleOption: (n) => "Module " + n,
  };
  const ORGANISERS = "ajm9311@ou.edu,biplabendu.das@gmail.com";
  const DATA_URL = "../assets/attendance/lookup.json";

  // --- crypto (mirror of encrypt-lookup.mjs) -------------------------------
  const enc = new TextEncoder(), dec = new TextDecoder();
  const unb64 = (s) => Uint8Array.from(atob(s), (c) => c.charCodeAt(0));
  const hex = (u8) => Array.from(u8, (b) => b.toString(16).padStart(2, "0")).join("");
  const normEmail = (x) =>
    String(x).replace(/[ ​‌‍﻿]/g, " ").trim().toLowerCase();

  async function derive(email, salt, iterations) {
    const base = await crypto.subtle.importKey("raw", enc.encode(normEmail(email)),
                                               "PBKDF2", false, ["deriveBits"]);
    const bits = new Uint8Array(await crypto.subtle.deriveBits(
      { name: "PBKDF2", hash: "SHA-256", salt: enc.encode(salt), iterations }, base, 384));
    const key = await crypto.subtle.importKey("raw", bits.slice(16, 48), "AES-GCM",
                                              false, ["decrypt"]);
    return { id: hex(bits.slice(0, 16)), key };
  }

  let dataPromise = null;
  const loadData = () =>
    (dataPromise ??= fetch(DATA_URL, { cache: "no-cache" }).then((r) => {
      if (!r.ok) throw new Error("HTTP " + r.status);
      return r.json();
    }));

  async function lookup(email) {
    const data = await loadData();
    const { id, key } = await derive(email, data.salt, data.iterations);
    const e = data.entries[id];
    if (!e) return { data, record: null };
    const pt = await crypto.subtle.decrypt({ name: "AES-GCM", iv: unb64(e.iv) }, key, unb64(e.ct));
    return { data, record: JSON.parse(dec.decode(pt)) };
  }

  // --- links ---------------------------------------------------------------
  function formLink(email, rec) {
    if (!FORM.url) return null;
    const p = new URLSearchParams({ usp: "pp_url" });
    const f = FORM.fields;
    if (f.email) p.append(f.email, normEmail(email));
    if (rec) {
      if (f.name && rec.name) p.append(f.name, rec.name);
      const others = rec.emails.filter((x) => x !== normEmail(email));
      if (f.otherEmails && others.length) p.append(f.otherEmails, others.join(", "));
      if (f.modules) rec.modules.filter((m) => m.attended)
        .forEach((m) => p.append(f.modules, FORM.moduleOption(m.n)));
    }
    return FORM.url + "?" + p.toString();
  }

  function mailLink(email, rec) {
    const lines = ["Email I looked up with: " + normEmail(email)];
    if (rec) {
      lines.push("Name on record: " + (rec.name || "(none)"),
        "Correct name for my certificate: ",
        "Other emails on record: " + (rec.emails.filter((x) => x !== normEmail(email)).join(", ") || "(none)"),
        "Other emails I used: ",
        "", "Attendance on record:");
      rec.modules.filter((m) => !m.upcoming).forEach((m) =>
        lines.push("  " + m.label + ": " + (m.attended ? "attended, " + m.minutes + " min" : "not recorded")));
      lines.push("Corrections: ");
    } else {
      lines.push("My email was not found. Name: ", "Modules I attended: ");
    }
    lines.push("", "Will I attend Module 4 on Nov 12 (Yes / No / Not sure): ");
    return "mailto:" + ORGANISERS +
      "?subject=" + encodeURIComponent("R workshop: attendance check + Module 4 RSVP") +
      "&body=" + encodeURIComponent(lines.join("\n"));
  }

  // --- rendering (textContent only: record text never becomes HTML) -------
  const el = (tag, attrs = {}, ...kids) => {
    const n = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) n.setAttribute(k, v);
    kids.forEach((k) => n.append(k));
    return n;
  };

  function actionButton(email, rec, label) {
    const href = formLink(email, rec) || mailLink(email, rec);
    const a = el("a", { class: "btn btn-primary", href }, label);
    if (FORM.url) { a.target = "_blank"; a.rel = "noopener"; }
    return a;
  }

  function renderFound(out, email, rec) {
    const others = rec.emails.filter((x) => x !== normEmail(email));
    const tbody = el("tbody");
    rec.modules.forEach((m) => {
      const status = m.upcoming ? "Upcoming"
                   : m.attended ? "✓ Attended" : "✗ Not recorded";
      tbody.append(el("tr", {},
        el("td", {}, m.label),
        el("td", {}, m.date),
        el("td", { class: m.upcoming ? "text-muted" : m.attended ? "text-success" : "text-danger" }, status),
        el("td", { class: "text-end" }, m.upcoming || !m.attended ? "—" : String(m.minutes))));
    });
    const held = rec.modules.filter((m) => !m.upcoming);
    const n = held.filter((m) => m.attended).length;

    out.replaceChildren(
      el("div", { class: "card attendance-card" },
        el("div", { class: "card-body" },
          el("h3", { class: "h5 card-title" }, "Your record"),
          el("dl", { class: "attendance-dl" },
            el("dt", {}, "Name on record"), el("dd", {}, rec.name || "(no name on record)"),
            el("dt", {}, "Other emails on record"),
            el("dd", {}, others.length ? others.join(", ") : "None")),
          el("p", {}, "Attended " + n + " of " + held.length + " sessions held so far."),
          el("div", { class: "table-responsive" },
            el("table", { class: "table table-sm" },
              el("thead", {}, el("tr", {},
                el("th", {}, "Module"), el("th", {}, "Date"),
                el("th", {}, "Status"), el("th", { class: "text-end" }, "Minutes"))),
              tbody)),
          el("p", {}, "Whether or not everything above is right, please use the button below. " +
            "Use it to correct anything that is wrong (the details above are filled in for you), " +
            "and to tell us whether you will attend Module 4."),
          actionButton(email, rec, "Confirm or correct + RSVP for Module 4"))));
  }

  function renderMissing(out, email) {
    out.replaceChildren(
      el("div", { class: "alert alert-warning" },
        el("p", {}, "We have no record under that email address. If you registered or joined " +
          "Zoom with a different address (a personal one, say), try that one instead."),
        el("p", { class: "mb-2" }, "Still not found? Use the button below so we can add you, " +
          "and tell us about Module 4 at the same time."),
        actionButton(email, null, "Tell us which modules you attended + RSVP")));
  }

  function init() {
    const form = document.getElementById("attendance-form");
    if (!form) return;
    const input = document.getElementById("attendance-email");
    const out = document.getElementById("attendance-result");
    const btn = form.querySelector("button");
    if (!window.crypto || !crypto.subtle) {
      out.textContent = "Your browser cannot run the lookup (it needs a secure https connection " +
        "and a modern browser). Please email the organisers instead.";
      return;
    }
    form.addEventListener("submit", async (ev) => {
      ev.preventDefault();
      const email = input.value;
      if (!/^[^@\s]+@[^@\s]+\.[^@\s]+$/.test(normEmail(email))) {
        out.replaceChildren(el("div", { class: "alert alert-secondary" },
          "That doesn't look like an email address."));
        return;
      }
      btn.disabled = true;
      out.replaceChildren(el("p", { class: "text-muted" }, "Looking up…"));
      try {
        const { record } = await lookup(email);
        record ? renderFound(out, email, record) : renderMissing(out, email);
      } catch (e) {
        out.replaceChildren(el("div", { class: "alert alert-danger" },
          "The lookup failed to load. Please reload the page or email the organisers."));
      } finally {
        btn.disabled = false;
      }
    });
    loadData().then((d) => {
      const u = document.getElementById("attendance-updated");
      if (u && d.updated) u.textContent = d.updated;
    }).catch(() => {});
  }

  document.readyState === "loading"
    ? document.addEventListener("DOMContentLoaded", init) : init();
})();
