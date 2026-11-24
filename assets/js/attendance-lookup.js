// attendance-lookup.js -- participant self-check for content/attendance.qmd
//
// Reads ../assets/attendance/lookup.json, which holds ONLY ciphertext. Each
// participant's record is AES-GCM encrypted under a key derived from their own
// email address, so a record can only be opened by someone who types that
// address. The file is generated privately (dev/zoom-attendance/encrypt-lookup.mjs);
// the scheme below must stay in step with that script.
//
// The end-of-workshop attendance check is done: participants who spot a problem
// report it with the button, which opens a pre-filled email to the organisers.
// FORM.url is intentionally empty so the page uses that email (formLink is kept
// only as the fallback switch; set FORM.url to re-enable a Google Form).
(function () {
  "use strict";

  // --- Google Form config (disabled: url empty -> email is used) ----------
  const FORM = {
    url: "",
    fields: {},
    moduleOption: (n) => "Module " + n,
  };
  const ORGANISERS = "biplabendu.das@gmail.com,ajm9311@ou.edu";
  const DATA_URL = "../assets/attendance/lookup.json";
  // Step 2: the feedback form, which also confirms the name for the certificate.
  // The button pre-fills the form's "Email you used for the workshop" question
  // (entry id below) with the address the participant looked up, so each response
  // can be matched back to an attendance record.
  const FEEDBACK_URL =
    "https://docs.google.com/forms/d/e/1FAIpQLScIijc_VR9vu8Oflb8Mkk_VxQNG7JNQXMiL8dti-VFvexfdkg/viewform";
  const FEEDBACK_EMAIL_ENTRY = "entry.261194170";

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
    const lines = ["Email I looked up with: " + normEmail(email), ""];
    lines.push("--- Attendance summary shown on the website ---");
    if (rec) {
      const held = rec.modules.filter((m) => !m.upcoming);
      const n = held.filter((m) => m.attended).length;
      lines.push("Name on record: " + (rec.name || "(none)"),
        "Other emails on record: " +
          (rec.emails.filter((x) => x !== normEmail(email)).join(", ") || "(none)"),
        "Attended " + n + " of " + held.length + " sessions.");
      held.forEach((m) =>
        lines.push("  " + m.label + " (" + m.date + "): " +
          (m.attended ? "attended, " + m.minutes + " min" : "not recorded")));
    } else {
      lines.push("No record was found for this email address.");
    }
    lines.push("", "--- Describe your issue ---", "");
    return "mailto:" + ORGANISERS +
      "?subject=" + encodeURIComponent("R + Stats Workshop - Attendance Issue") +
      "&body=" + encodeURIComponent(lines.join("\n"));
  }

  // --- rendering (textContent only: record text never becomes HTML) -------
  const el = (tag, attrs = {}, ...kids) => {
    const n = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) n.setAttribute(k, v);
    kids.forEach((k) => n.append(k));
    return n;
  };

  function actionButton(email, rec, label, cls) {
    const href = formLink(email, rec) || mailLink(email, rec);
    const a = el("a", { class: cls || "btn btn-primary", href }, label);
    if (FORM.url) { a.target = "_blank"; a.rel = "noopener"; }
    return a;
  }

  function feedbackButton(email, label) {
    let href = FEEDBACK_URL;
    if (FEEDBACK_EMAIL_ENTRY && email) {
      const p = new URLSearchParams({ usp: "pp_url" });
      p.append(FEEDBACK_EMAIL_ENTRY, normEmail(email));
      href += "?" + p.toString();
    }
    const a = el("a", { class: "btn btn-primary", href }, label);
    a.target = "_blank";
    a.rel = "noopener";
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
          el("p", {}, "Attended " + n + " of " + held.length + " sessions."),
          el("div", { class: "table-responsive" },
            el("table", { class: "table table-sm" },
              el("thead", {}, el("tr", {},
                el("th", {}, "Module"), el("th", {}, "Date"),
                el("th", {}, "Status"), el("th", { class: "text-end" }, "Minutes"))),
              tbody)),
          el("p", {}, "Now that you can see your record, two things:"),
          el("ol", { class: "attendance-steps" },
            el("li", {}, "If anything above is wrong, reach out so we can fix it before your " +
              "certificate is issued — the email opens with this summary filled in, so just " +
              "describe the problem."),
            el("li", {}, "If it looks right, open the feedback form to confirm your name for the " +
              "certificate and leave us a few comments.")),
          el("div", { class: "d-flex flex-wrap gap-2" },
            actionButton(email, rec, "If incorrect, click here to reach us.", "btn btn-outline-secondary"),
            feedbackButton(email, "Looks good — leave feedback")))));
  }

  function renderMissing(out, email) {
    out.replaceChildren(
      el("div", { class: "alert alert-warning" },
        el("p", {}, "We have no record under that email address. If you registered or joined " +
          "Zoom with a different address (a personal one, say), try that one instead."),
        el("p", { class: "mb-2" }, "Still not found? Use the button below so we can look into it " +
          "and add your attendance."),
        actionButton(email, null, "If incorrect, click here to reach us.")));
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
