class MyEventEmitter {
  /**@type {Map<string, Array<Function>} */
  #eventListeners;
  /**@type {Map<string, Array<Function>} */
  #eventListenersOnce;

  constructor() {
    this.#eventListeners = new Map();
    this.#eventListenersOnce = new Map();
  }

  on(event, listener) {
    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) {
      this.#eventListeners.set(String(event), [listener]);
      return this;
    }

    this.#eventListeners.set(String(event), [...listeners, listener]);

    return this;
  }

  emit(event, ...values) {
    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) return;

    listeners.forEach((listener) => listener(...values));

    return this;
  }

  #validateListener(cb) {
    if (typeof cb !== "function") {
      throw new Error(`Listener should be a function, received ${cb}`);
    }
  }
}

module.exports = MyEventEmitter;
