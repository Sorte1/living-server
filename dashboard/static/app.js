// Living Server Dashboard - Frontend

let pendingCode = null;
let isGenerating = false;

// --- Initialization ---

document.addEventListener('DOMContentLoaded', () => {
  checkStatus();
  loadRoutes();
  loadHistory();
  autoResizeTextarea();

  // Poll status and routes
  setInterval(checkStatus, 5000);
  setInterval(loadRoutes, 5000);
});

// --- Auto-resize textarea ---

function autoResizeTextarea() {
  const textarea = document.getElementById('user-input');
  textarea.addEventListener('input', () => {
    textarea.style.height = 'auto';
    textarea.style.height = Math.min(textarea.scrollHeight, 120) + 'px';
  });
}

// --- Server Status ---

async function checkStatus() {
  try {
    const resp = await fetch('/api/status');
    const data = await resp.json();
    const dot = document.getElementById('status-dot');
    const text = document.getElementById('status-text');

    if (data.running && data.connected) {
      dot.className = 'dot dot-on';
      text.textContent = 'Running';
    } else if (data.running) {
      dot.className = 'dot dot-connecting';
      text.textContent = 'Starting...';
    } else {
      dot.className = 'dot dot-off';
      text.textContent = 'Stopped';
    }
  } catch {
    document.getElementById('status-dot').className = 'dot dot-off';
    document.getElementById('status-text').textContent = 'Disconnected';
  }
}

// --- Server Controls ---

async function serverAction(action) {
  const buttons = document.querySelectorAll('.server-controls button');
  buttons.forEach(b => b.disabled = true);

  try {
    addSystemMessage(`Server ${action}...`);
    const resp = await fetch(`/api/${action}`, { method: 'POST' });
    const data = await resp.json();
    if (!data.success) {
      addSystemMessage('Server action failed: ' + (data.error || 'Unknown error'), true);
      buttons.forEach(b => b.disabled = false);
      return;
    }

    if (action === 'restart' || action === 'start') {
      // Poll until connected (SBCL takes a few seconds to start)
      addSystemMessage('Waiting for server to come up...');
      let connected = false;
      for (let i = 0; i < 15; i++) {
        await new Promise(r => setTimeout(r, 2000));
        try {
          const statusResp = await fetch('/api/status');
          const statusData = await statusResp.json();
          await checkStatus();
          if (statusData.connected) {
            connected = true;
            break;
          }
        } catch { /* keep trying */ }
      }
      if (connected) {
        addSystemMessage('Server is running and connected.');
        await loadRoutes();
      } else {
        addSystemMessage('Server started but Swank connection not yet established. It may still be loading.', true);
      }
    } else {
      await checkStatus();
      await loadRoutes();
      addSystemMessage(`Server ${action} completed.`);
    }
  } catch (err) {
    addSystemMessage('Failed to contact dashboard: ' + err.message, true);
  }

  buttons.forEach(b => b.disabled = false);
}

// --- Routes ---

async function loadRoutes() {
  try {
    const resp = await fetch('/api/routes');
    const data = await resp.json();
    const container = document.getElementById('routes-list');

    if (!data.routes || data.routes.length === 0) {
      container.innerHTML = '<div class="empty-state">No routes loaded</div>';
      return;
    }

    container.innerHTML = data.routes.map(route => {
      const methods = route.methods || ['GET'];
      const path = route.path || '/';
      return methods.map(method =>
        `<div class="route-item" onclick="testRoute('${method}', '${path}')">
          <span class="route-method method-${method}">${method}</span>
          <span class="route-path">${path}</span>
        </div>`
      ).join('');
    }).join('');
  } catch {
    // Silently fail - status will show disconnected
  }
}

function testRoute(method, path) {
  if (method === 'GET') {
    window.open(`http://localhost:3001${path}`, '_blank');
  } else {
    addSystemMessage(`Testing ${method} ${path} - use curl or a REST client for non-GET routes`);
  }
}

// --- Chat History ---

async function loadHistory() {
  try {
    const resp = await fetch('/api/history');
    const data = await resp.json();
    if (data.messages && data.messages.length > 0) {
      // Remove welcome message
      const welcome = document.querySelector('.welcome-message');
      if (welcome) welcome.remove();

      data.messages.forEach(msg => {
        if (msg.role === 'user') {
          addUserMessage(msg.content);
        } else if (msg.role === 'assistant') {
          addAssistantMessage(msg.content);
        }
      });
    }
  } catch {
    // Silently fail on first load
  }
}

// --- Chat ---

function handleKeydown(event) {
  if (event.key === 'Enter' && !event.shiftKey) {
    event.preventDefault();
    sendMessage();
  }
}

async function sendMessage() {
  const input = document.getElementById('user-input');
  const message = input.value.trim();
  if (!message || isGenerating) return;

  // Clear welcome message
  const welcome = document.querySelector('.welcome-message');
  if (welcome) welcome.remove();

  // Add user message
  addUserMessage(message);
  input.value = '';
  input.style.height = 'auto';

  // Show loading
  isGenerating = true;
  document.getElementById('btn-send').disabled = true;
  const loadingEl = addLoading();

  try {
    const resp = await fetch('/api/generate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ message })
    });

    const data = await resp.json();
    loadingEl.remove();

    if (data.error) {
      addSystemMessage('Error: ' + data.error, true);
    } else {
      // Show explanation
      addAssistantMessage(data.explanation || data.rawResponse);

      // Show code preview if code was generated
      if (data.code) {
        pendingCode = data.code;
        showCodePreview(data.code);
      }
    }
  } catch (err) {
    loadingEl.remove();
    addSystemMessage('Failed to contact dashboard: ' + err.message, true);
  }

  isGenerating = false;
  document.getElementById('btn-send').disabled = false;
}

// --- Code Preview ---

function showCodePreview(code) {
  const preview = document.getElementById('code-preview');
  const content = document.getElementById('code-content');
  content.textContent = code;
  preview.classList.remove('hidden');
}

function hideCodePreview() {
  document.getElementById('code-preview').classList.add('hidden');
  pendingCode = null;
}

async function confirmCode() {
  if (!pendingCode) return;

  const code = pendingCode;
  hideCodePreview();

  addSystemMessage('Running code...');

  try {
    const resp = await fetch('/api/confirm', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ code })
    });

    const data = await resp.json();
    if (data.success) {
      addSystemMessage('Code executed successfully! ' + (data.output || ''));
      await loadRoutes();
    } else {
      addSystemMessage('Execution failed: ' + (data.error || 'Unknown error'), true);
    }
  } catch (err) {
    addSystemMessage('Failed to execute code: ' + err.message, true);
  }
}

function cancelCode() {
  hideCodePreview();
  addSystemMessage('Code cancelled. You can ask me to try a different approach.');
}

// --- Message Rendering ---

function addUserMessage(text) {
  const messages = document.getElementById('messages');
  const div = document.createElement('div');
  div.className = 'message';
  div.innerHTML = `<div class="message-user">${escapeHtml(text)}</div>`;
  messages.appendChild(div);
  scrollToBottom();
}

function addAssistantMessage(text) {
  const messages = document.getElementById('messages');
  const div = document.createElement('div');
  div.className = 'message';
  div.innerHTML = `<div class="message-assistant">${formatMessage(text)}</div>`;
  messages.appendChild(div);
  scrollToBottom();
}

function addSystemMessage(text, isError = false) {
  const messages = document.getElementById('messages');
  const div = document.createElement('div');
  div.className = 'message';
  const classes = `message-system ${isError ? 'message-error' : 'message-success'}`;
  div.innerHTML = `<div class="${classes}">${escapeHtml(text)}</div>`;
  messages.appendChild(div);
  scrollToBottom();
}

function addLoading() {
  const messages = document.getElementById('messages');
  const div = document.createElement('div');
  div.className = 'message loading-message';
  div.innerHTML = `<div class="loading"><span></span><span></span><span></span></div>`;
  messages.appendChild(div);
  scrollToBottom();
  return div;
}

function scrollToBottom() {
  const messages = document.getElementById('messages');
  messages.scrollTop = messages.scrollHeight;
}

// --- Utilities ---

function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

function formatMessage(text) {
  // Simple markdown-like formatting
  return text
    .split('\n\n')
    .map(para => `<p>${escapeHtml(para)}</p>`)
    .join('');
}
