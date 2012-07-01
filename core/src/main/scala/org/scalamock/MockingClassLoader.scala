// Copyright (c) 2011-2012 Paul Butcher
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package org.scalamock

import java.io.ByteArrayOutputStream
import java.net.{URL, URLClassLoader}
import collection.mutable.ListMap
import org.objectweb.asm.{ClassReader, ClassWriter}
import org.objectweb.asm.commons.{Remapper, RemappingClassAdapter}

class MockingClassLoader(factory: MockFactoryBase) extends ClassLoader {
  
  val defaultClassLoader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
  val urls = defaultClassLoader.getURLs

  override def loadClass(name: String): Class[_] = {
    if (useDefault(name)) {
      defaultClassLoader.loadClass(name)
    } else {
      loader.loadClassInternal(name)
    }
  }
  
  class ClassLoaderInternal extends URLClassLoader(urls) {

    override def loadClass(name: String): Class[_] = MockingClassLoader.this.loadClass(name)
  
    def registerMockObject(objectName: String, mock: AnyRef) {
      mockObjectMap += ((objectName, mock.getClass.getName))
    }
    
    private[scalamock] def loadClassInternal(name: String) = {
      mockObjectMap.get(name) match {
        case Some(mock) =>
          println(s"$name --> $mock")
          val className = nameAsPath(name)
          val mockName = nameAsPath(mock)
          val bytecode = getClassAsBytes(mockName)
          val remappedByteCode = renameClass(bytecode, mockName, className)
          defineClass(name, remappedByteCode, 0, remappedByteCode.length)

        case None => super.loadClass(name)
      }
    }
    
    private def nameAsPath(name: String) = name.replace('.', '/')

    private def getClassAsBytes(className: String) = {
      val in = getResourceAsStream(className + ".class")
      if (in != null) {
        val out = new ByteArrayOutputStream
        
        val buffer = new Array[Byte](1024)
        while (in.read(buffer) != -1)
          out.write(buffer)
        
        out.toByteArray
      } else {
        null
      }
    }
    
    private def renameClass(bytecode: Array[Byte], from: String, to: String) = {
      val classReader = new ClassReader(bytecode)
      val classWriter = new ClassWriter(classReader, 0)
      val remapper = new RenamingRemapper(from, to)
      classReader.accept(new RemappingClassAdapter(classWriter, remapper), 0)
      classWriter.toByteArray
    }
    
    private class RenamingRemapper(from: String, to: String) extends Remapper {
      override def map(typeName: String) = {
        if (typeName == from)
          to
        else
          typeName
      }
    }
  }

  private def useDefault(name: String) =
    name.startsWith("scala.") || 
    name.startsWith("java.") || 
    name.startsWith("org.scalatest.") ||
    name.startsWith("org.scalamock.")

  private val loader = new ClassLoaderInternal
  
  private val mockObjectMap = new ListMap[String, String]
}