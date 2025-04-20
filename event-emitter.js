class MyEventEmitter {
  /**@type {Map<string, Array<{Function}>} */
  #eventListeners;

  constructor() {
    this.#eventListeners = new Map();
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

  once(event, listener) {
    this.#validateListener(listener);

    const cb = (...args) => {
      listener(...args);
      this.removeListener(event, cb);
    };

    return this.on(event, cb);
  }

  emit(event, ...values) {
    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) return;

    listeners.forEach((listener) => listener(...values));

    return this;
  }

  removeListener(event, listener) {
    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) return;

    const index = listeners.indexOf(listener);

    if (index !== -1) {
      this.#eventListeners.set(String(event), listeners.toSpliced(index, 1));
    }

    return this;
  }

  removeAllListeners(event) {
    if (event === undefined) {
      this.#eventListeners = new Map();
    }

    this.#eventListeners.delete(String(event));

    return this;
  }

  listenerCount(event, listener) {
    if (listener === undefined) {
      return (this.#eventListeners.get(String(event)) || []).length;
    }

    this.#validateListener(listener);

    const listeners = this.#eventListeners.get(String(event));

    if (!listeners) return 0;

    return listeners.filter((l) => l === listener).length;
  }

  #validateListener(cb) {
    if (typeof cb !== "function") {
      throw new Error(`Listener should be a function, received ${cb}`);
    }
  }

  /**
   * Aliases
   */

  off(event, listener) {
    return this.removeListener(event, listener);
  }

  addListener(event, listener) {
    return this.on(event, listener);
  }
}

module.exports = MyEventEmitter;
